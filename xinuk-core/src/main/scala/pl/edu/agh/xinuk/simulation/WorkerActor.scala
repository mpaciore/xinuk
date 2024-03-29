package pl.edu.agh.xinuk.simulation

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import org.slf4j.{Logger, LoggerFactory, MarkerFactory}
import pl.edu.agh.xinuk.algorithm._
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._

import java.awt.Color
import java.security.SecureRandom
import scala.collection.mutable
import scala.util.Random

class WorkerActor[ConfigType <: XinukConfig](
  regionRef: => ActorRef,
  planCreator: PlanCreator[ConfigType],
  planResolver: PlanResolver[ConfigType],
  emptyMetrics: => Metrics,
  signalPropagation: SignalPropagation,
  cellToColor: PartialFunction[CellState, Color]
)(implicit config: ConfigType) extends Actor with Stash {

  import pl.edu.agh.xinuk.simulation.WorkerActor._

  val guiActors: mutable.Set[ActorRef] = mutable.Set.empty
  val plansStash: mutable.Map[Long, Seq[Seq[TargetedPlan]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val consequencesStash: mutable.Map[Long, Seq[Seq[TargetedUpdate]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val signalUpdatesStash: mutable.Map[Long, Seq[Seq[(CellId, SignalMap)]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val remoteCellContentsStash: mutable.Map[Long, Seq[Seq[(CellId, CellContents)]]] = mutable.Map.empty.withDefaultValue(Seq.empty)

  val random: Random = new SecureRandom

  var logger: Logger = _
  var id: WorkerId = _
  var worldShard: WorldShard = _
  var iterationMetrics: Metrics = _
  var currentIteration: Long = _
  var iterationFinished: Boolean = _

  var lastStepEnd: Long = _
  var iterationActiveDuration: Long = _
  var iterationWaitingDuration: Long = _

  override def receive: Receive = stopped

  def stopped: Receive = {

    case SubscribeGridInfo() =>
      guiActors += sender()

    case WorkerInitialized(world) =>
      this.id = world.workerId
      this.worldShard = world
      this.logger = LoggerFactory.getLogger(id.value.toString)
      logger.info("starting")
      planCreator.initialize(worldShard)
      lastStepEnd = System.nanoTime
      self ! StartIteration(1)
      unstashAll()
      context.become(started)

    case _ =>
      stash()
  }

  def started: Receive = {

    case SubscribeGridInfo() =>
      guiActors += sender()

    case StartIteration(iteration) if iteration > config.iterationsNumber =>
      logger.info("finalizing")
      planCreator.finalize(worldShard)
      logger.info("terminating")
      import scala.concurrent.duration._
      Thread.sleep(5.seconds.toMillis)
      context.system.terminate()

    case StartIteration(iteration) =>
      measureTimes {
        currentIteration = iteration
        iterationFinished = false
        iterationMetrics = emptyMetrics
        val plans: Seq[TargetedPlan] = worldShard.localCellIds.map(worldShard.cells(_)).flatMap(createPlans).toSeq
        distributePlans(currentIteration, plans)
      }

    case RemotePlans(iteration, remotePlans) =>
      measureTimes {
      plansStash(iteration) :+= remotePlans
      if (plansStash(currentIteration).size == worldShard.incomingWorkerNeighbours.size) {
        val shuffledPlans: Seq[TargetedPlan] = shuffleUngroup(flatGroup(plansStash(currentIteration))(_.action.target))
        val (acceptedPlans, discardedPlans) = processPlans(shuffledPlans)
        plansStash.remove(currentIteration)

        distributeConsequences(currentIteration, acceptedPlans.flatMap(_.consequence) ++ discardedPlans.flatMap(_.alternative))
      }
    }

    case RemoteConsequences(iteration, remoteConsequences) =>
      measureTimes {
        consequencesStash(iteration) :+= remoteConsequences
        if (consequencesStash(currentIteration).size == worldShard.incomingWorkerNeighbours.size) {
          val consequences: Seq[TargetedUpdate] = flatGroup(consequencesStash(currentIteration))(_.target).flatMap(_._2).toSeq
          consequences.foreach(applyUpdate)
          consequencesStash.remove(currentIteration)

          val signalUpdates = if (config.signalDisabled) Map.empty[CellId, SignalMap] else calculateSignalUpdates()
          distributeSignal(currentIteration, signalUpdates)
        }
      }

    case RemoteSignal(iteration, remoteSignalUpdates) =>
      measureTimes {
        signalUpdatesStash(iteration) :+= remoteSignalUpdates
        if (signalUpdatesStash(currentIteration).size == worldShard.incomingWorkerNeighbours.size) {
          val signalUpdates: Map[CellId, SignalMap] = flatGroup(signalUpdatesStash(currentIteration))(_._1).map {
            case (id, groups) => (id, groups.map(_._2).reduce(_ + _))
          }
          applySignalUpdates(signalUpdates)
          signalUpdatesStash.remove(currentIteration)

          distributeRemoteCellContents(currentIteration)
        }
      }

    case RemoteCellContents(iteration, remoteCellContents) =>
      measureTimes {
        remoteCellContentsStash(iteration) :+= remoteCellContents
        if (remoteCellContentsStash(currentIteration).size == worldShard.outgoingWorkerNeighbours.size) {
          remoteCellContentsStash(currentIteration).flatten.foreach({
            case (cellId, cellContents) => worldShard.cells(cellId).updateContents(cellContents)
          })
          remoteCellContentsStash.remove(currentIteration)

          if (guiActors.nonEmpty && iteration >= config.guiStartIteration && (iteration - config.guiStartIteration) % config.guiUpdateFrequency == 0) {
            val cellColors = cellsToColors(worldShard.localCellIds.map(worldShard.cells(_)))
            guiActors.foreach(_ ! GridInfo(iteration, cellColors, iterationMetrics))
          }
          if (iteration % config.iterationFinishedLogFrequency == 0) {
            logger.info(s"finished $iteration")
          }
          self ! StartIteration(currentIteration + 1)
          iterationFinished = true
        }
      }

      if (iterationFinished) {
        if (!config.skipEmptyLogs || iterationMetrics != emptyMetrics) {
          logMetrics(currentIteration, iterationMetrics)
        }
        iterationActiveDuration = 0L
        iterationWaitingDuration = 0L
      }
  }

  private def measureTimes[A](block: => A): A = {
    val beginning = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    iterationActiveDuration += end - beginning
    iterationWaitingDuration += beginning - lastStepEnd
    lastStepEnd = end
    result
  }

  private def createPlans(cell: Cell): Seq[TargetedPlan] = {
    val neighbourStates = worldShard.cellNeighbours(cell.id)
      .map { case (direction, neighbourId) => (direction, worldShard.cells(neighbourId).state.contents) }
    val (plans, metrics) = planCreator.createPlans(currentIteration, cell.id, cell.state, neighbourStates)
    iterationMetrics += metrics
    plans.outwardsPlans.flatMap {
      case (direction, plans) =>
        val planSource = cell.id
        val actionTarget = worldShard.cellNeighbours(cell.id)(direction)
        val consequenceTarget = cell.id
        val alternativeTarget = cell.id
        plans.map(_.toTargeted(planSource, actionTarget, consequenceTarget, alternativeTarget))
    }.toSeq ++ plans.localPlans.map(_.toTargeted(cell.id, cell.id, cell.id, cell.id))
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
    val target = worldShard.cells(plan.action.target)
    val action = plan.action.update
    planResolver.isUpdateValid(currentIteration, target.state.contents, action)
  }

  private def applyUpdate(stateUpdate: TargetedUpdate): Unit = {
    val target = worldShard.cells(stateUpdate.target)
    val action = stateUpdate.update
    val (result, metrics) = planResolver.applyUpdate(currentIteration, target.state.contents, action)
    target.updateContents(result)
    iterationMetrics += metrics
  }

  private def calculateSignalUpdates(): Map[CellId, SignalMap] = {
    worldShard.calculateSignalUpdates(currentIteration, signalPropagation)
  }

  private def applySignalUpdates(signalUpdates: Map[CellId, SignalMap]): Unit = {
    signalUpdates.foreach {
      case (cellId, signalUpdate) =>
        val targetCell = worldShard.cells(cellId)
        val oldSignal = targetCell.state.signalMap
        val newSignal = (oldSignal + signalUpdate * config.signalSuppressionFactor) * config.signalAttenuationFactor
        targetCell.updateSignal(newSignal * targetCell.state.contents.signalFactor(currentIteration))
    }
  }

  private def distributePlans(iteration: Long, plansToDistribute: Seq[TargetedPlan]): Unit = {
    val grouped = groupByWorker(plansToDistribute) { plan => plan.action.target }
    distribute(
      worldShard.outgoingWorkerNeighbours, grouped)(
      Seq.empty, { data => RemotePlans(iteration, data) })
  }

  private def distribute[A](keys: Set[WorkerId], groups: Map[WorkerId, A])(default: => A, msgCreator: A => Any): Unit = {
    keys.foreach { workerId =>
      send(regionRef, workerId, msgCreator(groups.getOrElse(workerId, default)))
    }
  }

  private def groupByWorker[A](items: Seq[A])(idExtractor: A => CellId): Map[WorkerId, Seq[A]] = {
    items.groupBy { item => worldShard.cellToWorker(idExtractor(item)) }
  }

  private def distributeConsequences(iteration: Long, consequencesToDistribute: Seq[TargetedUpdate]): Unit = {
    val grouped = groupByWorker(consequencesToDistribute) { update => update.target }
    distribute(
      worldShard.outgoingWorkerNeighbours, grouped)(
      Seq.empty, { data => RemoteConsequences(iteration, data) })
  }

  private def distributeSignal(iteration: Long, signalToDistribute: Map[CellId, SignalMap]): Unit = {
    val grouped = groupByWorker(signalToDistribute.toSeq) { case (id, _) => id }
    distribute(
      worldShard.outgoingWorkerNeighbours, grouped)(
      Seq.empty, { data => RemoteSignal(iteration, data) })
  }

  private def distributeRemoteCellContents(iteration: Long): Unit = {
    distribute(
      worldShard.incomingWorkerNeighbours, worldShard.incomingCells)(
      Set.empty, { data => RemoteCellContents(iteration, data.toSeq.map(id => (id, worldShard.cells(id).state.contents))) })

  }

  private def defaultColor: CellState => Color =
    state => state.contents match {
      case Obstacle => Color.BLACK
      case Empty => Color.WHITE
      case other =>
        val random = new Random(other.getClass.hashCode())
        val hue = random.nextFloat()
        val saturation = 1.0f
        val luminance = 0.6f
        Color.getHSBColor(hue, saturation, luminance)
    }

  private def cellsToColors(cells: Set[Cell]): Map[CellId, Color] = cells.map {
    cell => cell.id -> cellToColor.applyOrElse(cell.state, defaultColor)
  }.toMap

  private def logMetrics(iteration: Long, metrics: Metrics): Unit = {
    logger.info(WorkerActor.MetricsMarker, "{};{};{};{}", iteration, iterationActiveDuration, iterationWaitingDuration, metrics)
  }

  private def flatGroup[A](seqs: Seq[Seq[A]])(idExtractor: A => CellId): Map[CellId, Seq[A]] = {
    seqs.flatten.groupBy {
      idExtractor(_)
    }
  }

  private def shuffleUngroup[*, V](groups: Map[*, Seq[V]]): Seq[V] = {
    random.shuffle(groups.keys.toSeq).flatMap(k => random.shuffle(groups(k)))
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
                                       cellToColor: PartialFunction[CellState, Color]
                                      )(implicit config: ConfigType): Props = {
    Props(new WorkerActor(regionRef, planCreator, planResolver, emptyMetrics, signalPropagation, cellToColor))
  }

  def send(ref: ActorRef, id: WorkerId, msg: Any): Unit = ref ! MsgWrapper(id, msg)

  def extractShardId(implicit config: XinukConfig): ExtractShardId = {
    case MsgWrapper(id, _) => (id.value % config.shardingMod).toString
  }

  def extractEntityId: ExtractEntityId = {
    case MsgWrapper(id, msg) =>
      (id.value.toString, msg)
  }

  final case class MsgWrapper(id: WorkerId, value: Any)

  final case class SubscribeGridInfo()

  final case class GridInfo (iteration: Long, cellColors: Map[CellId, Color], metrics: Metrics)

  final case class WorkerInitialized(world: WorldShard)

  final case class StartIteration private(i: Long) extends AnyVal

  final case class RemotePlans private(iteration: Long, plans: Seq[TargetedPlan])

  final case class RemoteConsequences private(iteration: Long, consequences: Seq[TargetedUpdate])

  final case class RemoteSignal private(iteration: Long, signalUpdates: Seq[(CellId, SignalMap)])

  final case class RemoteCellContents private(iteration: Long, remoteCellContents: Seq[(CellId, CellContents)])

}