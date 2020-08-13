package pl.edu.agh.xinuk.simulation

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import org.slf4j.{Logger, LoggerFactory, MarkerFactory}
import pl.edu.agh.xinuk.algorithm._
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.gui.GuiActor.GridInfo
import pl.edu.agh.xinuk.model._

import scala.collection.mutable
import scala.util.Random

class WorkerActor[ConfigType <: XinukConfig](
  regionRef: => ActorRef,
  planCreator: PlanCreator[ConfigType],
  planResolver: PlanResolver[ConfigType],
  emptyMetrics: => Metrics,
  signalPropagation: SignalPropagation
)(implicit config: ConfigType) extends Actor with Stash {

  import pl.edu.agh.xinuk.simulation.WorkerActor._

  val guiActors: mutable.Set[ActorRef] = mutable.Set.empty
  val plansStash: mutable.Map[Long, Seq[Seq[TargetedPlan]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val consequencesStash: mutable.Map[Long, Seq[Seq[TargetedStateUpdate]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val signalUpdatesStash: mutable.Map[Long, Seq[Seq[(CellId, SignalMap)]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val remoteCellContentsStash: mutable.Map[Long, Seq[Seq[(CellId, CellContents)]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  var logger: Logger = _
  var id: WorkerId = _
  var world: World = _
  var iterationMetrics: Metrics = _
  var currentIteration: Long = _

  override def receive: Receive = stopped

  def stopped: Receive = {

    case SubscribeGridInfo(_) =>
      guiActors += sender()

    case WorkerInitialized(id, world) =>
      this.id = id
      this.world = world
      this.logger = LoggerFactory.getLogger(id.value.toString)
      logger.info(
        s"outgoing neighbours: ${world.outgoingWorkerNeighbours.map(_.value)} " +
        s"incoming neighbours: ${world.incomingWorkerNeighbours.map(_.value)}")
      self ! StartIteration(1)
      unstashAll()
      context.become(started)

    case _ =>
      stash()
  }

  def started: Receive = {

    case SubscribeGridInfo(_) =>
      guiActors += sender()

    case StartIteration(iteration) if iteration > config.iterationsNumber =>
      logger.info(s"$id terminating")
      import scala.concurrent.duration._
      Thread.sleep(5.seconds.toMillis)
      context.system.terminate()

    case StartIteration(iteration) =>
      currentIteration = iteration
      iterationMetrics = emptyMetrics
      val plans: Seq[TargetedPlan] = world.localCellIds.map(world.cells(_)).flatMap(createPlans).toSeq
      distributePlans(currentIteration, plans)

    case RemotePlans(_, iteration, remotePlans) =>
      plansStash(iteration) :+= remotePlans
      if (plansStash(currentIteration).size == world.incomingWorkerNeighbours.size) {
        val shuffledPlans: Seq[TargetedPlan] = shuffleUngroup(flatGroup(plansStash(currentIteration))(_.action.target))
        // discarded plans might be used for further retry rounds
        val (acceptedPlans, discardedPlans) = processPlans(shuffledPlans)
        plansStash.remove(currentIteration)

        distributeConsequences(currentIteration, acceptedPlans.flatMap(_.consequence))
      }

    case RemoteConsequences(_, iteration, remoteConsequences) =>
      consequencesStash(iteration) :+= remoteConsequences
      if (consequencesStash(currentIteration).size == world.incomingWorkerNeighbours.size) {
        val consequences: Seq[TargetedStateUpdate] = flatGroup(consequencesStash(currentIteration))(_.target).flatMap(_._2).toSeq
        consequences.foreach(applyUpdate)
        consequencesStash.remove(currentIteration)

        val signalUpdates = calculateSignalUpdates()
        distributeSignal(currentIteration, signalUpdates)
      }

    case RemoteSignal(_, iteration, remoteSignalUpdates) =>
      signalUpdatesStash(iteration) :+= remoteSignalUpdates.toSeq
      if (signalUpdatesStash(currentIteration).size == world.incomingWorkerNeighbours.size) {
        val signalUpdates: Map[CellId, SignalMap] = flatGroup(signalUpdatesStash(currentIteration))(_._1).map {
          case (id, groups) => (id, groups.map(_._2).reduce(_ + _))
        }
        applySignalUpdates(signalUpdates)
        signalUpdatesStash.remove(currentIteration)

        distributeRemoteCellContents(currentIteration)
      }

    case RemoteCellContents(_, iteration, remoteCellContents) =>
      remoteCellContentsStash(iteration) :+= remoteCellContents.toSeq
      if (remoteCellContentsStash(currentIteration).size == world.outgoingWorkerNeighbours.size) {
        remoteCellContentsStash(currentIteration).flatten.foreach({
          case (cellId, cellContents) => world.cells(cellId).updateContents(cellContents)
        })
        remoteCellContentsStash.remove(currentIteration)

        logMetrics(currentIteration, iterationMetrics)
        guiActors.foreach(_ ! GridInfo(iteration, world.localCellIds.map(world.cells(_)), iterationMetrics))
        if (iteration % 100 == 0) logger.info(s"$id finished $iteration")
        self ! StartIteration(currentIteration + 1)
      }
  }

  private def createPlans(cell: Cell): Seq[TargetedPlan] = {
    val neighbourStates = world.cellNeighbours(cell.id)
      .map { case (direction, neighbourId) => (direction, world.cells(neighbourId).state.contents) }
    val (localPlans, metrics) = planCreator.createPlans(currentIteration, cell.state, neighbourStates)
    iterationMetrics += metrics
    localPlans.flatMap {
      case (direction, plans) =>
        val actionTarget = world.cellNeighbours(cell.id)(direction)
        val consequenceTarget = cell.id
        plans.map {
          _.toTargeted(actionTarget, consequenceTarget)
        }
    }.toSeq
  }

  private def processPlans(plans: Seq[TargetedPlan]): (Seq[TargetedPlan], Seq[TargetedPlan]) = {
    plans.partition { plan =>
      if (validatePlan(plan)) {
        applyUpdate(plan.action)
        true
      } else {
        false
      }
    }
  }

  private def validatePlan(plan: TargetedPlan): Boolean = {
    val target = world.cells(plan.action.target)
    val action = plan.action.update
    planResolver.isUpdateValid(target.state, action)
  }

  private def applyUpdate(stateUpdate: TargetedStateUpdate): Unit = {
    val target = world.cells(stateUpdate.target)
    val action = stateUpdate.update
    val (result, metrics) = planResolver.applyUpdate(target.state, action)
    target.update(result)
    iterationMetrics += metrics
  }

  private def calculateSignalUpdates(): Map[CellId, SignalMap] = {
    world.calculateSignalUpdates(signalPropagation)
  }

  private def applySignalUpdates(signalUpdates: Map[CellId, SignalMap]): Unit = {
    signalUpdates.foreach {
      case (cellId, signalUpdate) =>
        val oldSignal = world.cells(cellId).state.signalMap
        val generatedSignal = world.cells(cellId).state.contents.passiveSignal()
        val newSignal = oldSignal * config.signalAttenuationFactor + signalUpdate * config.signalSuppressionFactor + generatedSignal
        world.cells(cellId).updateSignal(newSignal)
    }
  }

  private def distributePlans(iteration: Long, plansToDistribute: Seq[TargetedPlan]): Unit = {
    val grouped = groupByWorker(plansToDistribute) { plan => plan.action.target }
    distribute(
      world.outgoingWorkerNeighbours, grouped)(
      Seq.empty, { case (workerId, data) => RemotePlans(workerId, iteration, data) })
  }

  private def distribute[A](keys: Set[WorkerId], groups: Map[WorkerId, A])(default: => A, msgCreator: (WorkerId, A) => Any): Unit = {
    keys.foreach { workerId =>
      regionRef ! msgCreator(workerId, groups.getOrElse(workerId, default))
    }
  }

  private def groupByWorker[A](items: Seq[A])(idExtractor: A => CellId): Map[WorkerId, Seq[A]] = {
    items.groupBy { item => world.cellToWorker(idExtractor(item)) }
  }

  private def distributeConsequences(iteration: Long, consequencesToDistribute: Seq[TargetedStateUpdate]): Unit = {
    val grouped = groupByWorker(consequencesToDistribute) { update => update.target }
    distribute(
      world.outgoingWorkerNeighbours, grouped)(
      Seq.empty, { case (workerId, data) => RemoteConsequences(workerId, iteration, data) })
  }

  private def distributeSignal(iteration: Long, signalToDistribute: Map[CellId, SignalMap]): Unit = {
    val grouped = groupByWorker(signalToDistribute.toSeq) { case (id, _) => id }
    distribute(
      world.outgoingWorkerNeighbours, grouped)(
      Seq.empty, { case (workerId, data) => RemoteSignal(workerId, iteration, data) })
  }

  private def distributeRemoteCellContents(iteration: Long): Unit = {
    distribute(
      world.incomingWorkerNeighbours, world.incomingCells)(
      Set.empty, { case (workerId, data) => RemoteCellContents(workerId, iteration, data.toSeq.map(id => (id, world.cells(id).state.contents))) })

  }

  private def logMetrics(iteration: Long, metrics: Metrics): Unit = {
    logger.info(WorkerActor.MetricsMarker, "{};{}", iteration.toString, metrics: Any)
  }

  private def flatGroup[A](seqs: Seq[Seq[A]])(idExtractor: A => CellId): Map[CellId, Seq[A]] = {
    seqs.flatten.groupBy {
      idExtractor(_)
    }
  }

  private def shuffleUngroup[*, V](groups: Map[*, Seq[V]]): Seq[V] = {
    Random.shuffle(groups.keys.toList).flatMap(k => Random.shuffle(groups(k)))
  }
}

object WorkerActor {

  final val Name: String = "WorkerActor"

  final val MetricsMarker = MarkerFactory.getMarker("METRICS")

  def props[ConfigType <: XinukConfig](regionRef: => ActorRef,
                                       planCreator: PlanCreator[ConfigType],
                                       planResolver: PlanResolver[ConfigType],
                                       emptyMetrics: => Metrics,
                                       signalPropagation: SignalPropagation,
                                      )(implicit config: ConfigType): Props = {
    Props(new WorkerActor(regionRef, planCreator, planResolver, emptyMetrics, signalPropagation))
  }

  def extractShardId(implicit config: XinukConfig): ExtractShardId = {
    case SubscribeGridInfo(id) => idToShard(id)
    case WorkerInitialized(id, _) => idToShard(id)
    case RemotePlans(id, _, _) => idToShard(id)
    case RemoteConsequences(id, _, _) => idToShard(id)
    case RemoteSignal(id, _, _) => idToShard(id)
    case RemoteCellContents(id, _, _) => idToShard(id)
  }

  @inline private def idToShard(id: WorkerId)(implicit config: XinukConfig): String = (id.value % config.shardingMod).toString

  def extractEntityId: ExtractEntityId = {
    case msg@SubscribeGridInfo(id) =>
      (idToEntity(id), msg)
    case msg@WorkerInitialized(id, _) =>
      (idToEntity(id), msg)
    case msg@RemotePlans(id, _, _) =>
      (idToEntity(id), msg)
    case msg@RemoteConsequences(id, _, _) =>
      (idToEntity(id), msg)
    case msg@RemoteSignal(id, _, _) =>
      (idToEntity(id), msg)
    case msg@RemoteCellContents(id, _, _) =>
      (idToEntity(id), msg)
  }

  @inline private def idToEntity(id: WorkerId): String = id.value.toString

  final case class StartIteration private(i: Long) extends AnyVal

  final case class WorkerInitialized(id: WorkerId, world: World)

  final case class SubscribeGridInfo(id: WorkerId)

  final case class RemotePlans private(id: WorkerId, iteration: Long, plans: Seq[TargetedPlan])

  final case class RemoteConsequences private(id: WorkerId, iteration: Long, consequences: Seq[TargetedStateUpdate])

  final case class RemoteSignal private(id: WorkerId, iteration: Long, signalUpdates: Seq[(CellId, SignalMap)])

  final case class RemoteCellContents private(id: WorkerId, iteration: Long, remoteCellContents: Seq[(CellId, CellContents)])

}