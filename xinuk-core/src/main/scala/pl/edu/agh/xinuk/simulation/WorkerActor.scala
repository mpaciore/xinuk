package pl.edu.agh.xinuk.simulation

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import org.slf4j.{Logger, LoggerFactory, MarkerFactory}
import pl.edu.agh.xinuk.algorithm.{Move, MovePlan, PlanCreator, PlanResolver}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.gui.GuiActor.GridInfo
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.EnhancedCell.NeighbourMap
import pl.edu.agh.xinuk.model._

import scala.collection.mutable
import scala.util.Random

class WorkerActor[ConfigType <: XinukConfig](
  regionRef: => ActorRef,
  planCreator: PlanCreator[ConfigType],
  planResolver: PlanResolver[ConfigType],
  emptyMetricsFactory: () => Metrics,
  smellPropagationFunction: (EnhancedGrid, NeighbourMap) => SmellMap
)(implicit config: ConfigType) extends Actor with Stash {

  import pl.edu.agh.xinuk.simulation.WorkerActor._

  var logger: Logger = _
  var id: WorkerId = _
  var outgoingNeighbours: Set[WorkerId] = _
  var incomingNeighbours: Set[WorkerId] = _
  val guiActors: mutable.Set[ActorRef] = mutable.Set.empty

  var grid: EnhancedGrid = _
  var iterationMetrics: Metrics = emptyMetricsFactory()
  var currentIteration: Long = 1

  val plans: mutable.Map[Long, Seq[Seq[MovePlan]]] = mutable.Map.empty.withDefaultValue(Seq.empty)
  val consequences: mutable.Map[Long, Seq[Seq[Move]]] = mutable.Map.empty.withDefaultValue(Seq.empty)

  override def receive: Receive = stopped

  def stopped: Receive = {

    case SubscribeGridInfo(_) =>
      guiActors += sender()

    case NeighboursInitialized(id, grid, outgoingNeighbours, incomingNeighbours) =>
      this.id = id
      this.grid = grid
      this.outgoingNeighbours = outgoingNeighbours
      this.incomingNeighbours = incomingNeighbours
      this.logger = LoggerFactory.getLogger(id.value.toString)
      logger.info(s"${id.value} " +
        s"outgoing neighbours: ${outgoingNeighbours.map(_.value)} " +
        s"incoming neighbours: ${incomingNeighbours.map(_.value)}")
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
      iterationMetrics = emptyMetricsFactory()

      val plans = grid.cellsWithNeighbours.map {
        case (cellCoords, cell, neighbourMap) => (cellCoords, cell, neighbourMap.map {
          case (direction, neighbourCoords) => (direction, (neighbourCoords, grid.getCellAt(neighbourCoords).cell))
        })
      }.flatMap {
        case (cellCoords, cell, extendedNeighbourMap) =>
          val (plans, metrics) = planCreator.createPlans(cellCoords, cell, extendedNeighbourMap)
          iterationMetrics += metrics
          plans
      }

      distributePlans(currentIteration, plans)

    case RemotePlans(_, _, iteration, remotePlans) =>
      plans(iteration) :+= remotePlans
      if (plans(currentIteration).size == incomingNeighbours.size) {
        val mergedPlans: Map[(Int, Int), Seq[MovePlan]] = flatGroup(plans(currentIteration)) {
          _.action.targetCoordinates
        }

        // could be filter instead of partition, but further passes using discarded plans are planned in future
        val (appliedPlans, discardedPlans) = shuffleUngroup(mergedPlans).partition {
          plan =>
            val targetCoordinates = plan.action.targetCoordinates
            val action = plan.action.update
            val target = grid.getCellAt(targetCoordinates).cell
            if (planResolver.isUpdateValid(action, target)) {
              val (result, metrics) = planResolver.applyUpdate(action, target)
              grid.setCellAt(targetCoordinates, result)
              iterationMetrics += metrics
              true
            } else {
              false
            }
        }
        plans.remove(currentIteration)
        distributeConsequences(currentIteration, appliedPlans.map(_.consequence))
      }

    case RemoteConsequences(_, _, iteration, remoteConsequences) =>
      consequences(iteration) :+= remoteConsequences
      if (consequences(currentIteration).size == incomingNeighbours.size) {
        val mergedConsequences: Map[(Int, Int), Seq[Move]] = flatGroup(consequences(currentIteration)) {
          _.targetCoordinates
        }
        mergedConsequences.flatMap(_._2).foreach {
          consequence =>
            val targetCoordinates = consequence.targetCoordinates
            val action = consequence.update
            val target = grid.getCellAt(targetCoordinates).cell
            val (result, metrics) = planResolver.applyUpdate(action, target)
            grid.setCellAt(targetCoordinates, result)
            iterationMetrics += metrics
        }

        //        propagate signal
        //        apply local signal
        //        send remote signal
        // TODO move after receiving signal:
        logMetrics(currentIteration, iterationMetrics)
        guiActors.foreach(_ ! GridInfo(iteration, grid, iterationMetrics))
        if (iteration % 100 == 0) logger.info(s"$id finished $iteration")
        self ! StartIteration(currentIteration + 1)

      }

//    case RemoteSignal(???) =>
//      apply remote signal
//      if (all signal received) {
//        logMetrics(currentIteration, iterationMetrics)
//        guiActors.foreach(_ ! GridInfo(iteration, grid, iterationMetrics))
//        if (iteration % 100 == 0) logger.info(s"$id finished $iteration")
//        self ! StartIteration(currentIteration + 1)
//      }
  }

  private def propagateSignal(): Unit = {
    (0 until config.signalSpeedRatio).foreach { _ => grid = grid.propagatedSignal(smellPropagationFunction) }
  }

  private def distributePlans(iteration: Long, plansToDistribute: Seq[MovePlan]): Unit = {
    val grouped = groupByWorker(plansToDistribute) {
      _.action.targetCoordinates
    }
    distribute(grouped)(Seq.empty, { case (workerId, data) => RemotePlans(id, workerId, iteration, data) })
  }

  private def distributeConsequences(iteration: Long, consequencesToDistribute: Seq[Move]): Unit = {
    val grouped = groupByWorker(consequencesToDistribute) {
      _.targetCoordinates
    }
    distribute(grouped)(Seq.empty, { case (workerId, data) => RemoteConsequences(id, workerId, iteration, data) })
  }

  private def distribute[A](groups: Map[WorkerId, A])(default: => A, msgCreator: (WorkerId, A) => Any): Unit = {
    outgoingNeighbours.foreach { workerId =>
      regionRef ! msgCreator(workerId, groups.getOrElse(workerId, default))
    }
  }

  private def logMetrics(iteration: Long, metrics: Metrics): Unit = {
    logger.info(WorkerActor.MetricsMarker, "{};{}", iteration.toString, metrics: Any)
  }

  private def flatGroup[A](seqs: Seq[Seq[A]])(coordExtractor: A => (Int, Int)): Map[(Int, Int), Seq[A]] = {
    seqs
      .flatten
      .groupBy { coordExtractor(_) }
  }

  private def shuffleUngroup[*, V](groups: Map[*, Seq[V]]): Seq[V] = {
    Random.shuffle(groups.keys.toList)
        .flatMap(k => Random.shuffle(groups(k)))
  }

  private def groupByWorker[A](items: Seq[A])(coordExtractor: A => (Int, Int)): Map[WorkerId, Seq[A]] = {
    items
      .groupBy {item =>
        val coords = coordExtractor(item)
        if (grid.isLocal(coords)) id else grid.getWorkerFor(coords)
      }
  }
}

object WorkerActor {

  final val Name: String = "WorkerActor"

  final val MetricsMarker = MarkerFactory.getMarker("METRICS")

  final case class NeighboursInitialized(id: WorkerId,
                                         grid: EnhancedGrid,
                                         outgoingNeighbours: Set[WorkerId],
                                         incomingNeighbours: Set[WorkerId])

  final case class StartIteration private(i: Long) extends AnyVal

  final case class SubscribeGridInfo(id: WorkerId)

  //sent to listeners
  final case class IterationPartFinished private(from: WorkerId,
                                                 to: WorkerId,
                                                 iteration: Long,
                                                 incomingBuffer: Set[((Int, Int), Cell)])

  final case class RemotePlans private(from: WorkerId,
                                       to: WorkerId,
                                       iteration: Long,
                                       plans: Seq[MovePlan])

  final case class RemoteConsequences private(from: WorkerId,
                                              to: WorkerId,
                                              iteration: Long,
                                              consequences: Seq[Move])

  final case class RemoteSignal(from: WorkerId,
                                to: WorkerId,
                                iteration: Long,
                                signal: Nothing)

  def props[ConfigType <: XinukConfig](regionRef: => ActorRef,
                                       planCreator: PlanCreator[ConfigType],
                                       planResolver: PlanResolver[ConfigType],
                                       emptyMetricsFactory: () => Metrics,
                                       smellPropagationFunction: (EnhancedGrid, NeighbourMap) => SmellMap
                                      )(implicit config: ConfigType): Props = {
    Props(new WorkerActor(regionRef, planCreator, planResolver, emptyMetricsFactory, smellPropagationFunction))
  }

  private def idToShard(id: WorkerId)(implicit config: XinukConfig): String = (id.value % config.shardingMod).toString

  def extractShardId(implicit config: XinukConfig): ExtractShardId = {
    case SubscribeGridInfo(id) => idToShard(id)
    case NeighboursInitialized(id, _, _, _) => idToShard(id)
    case RemotePlans(_, id, _, _) => idToShard(id)
    case RemoteConsequences(_, id, _, _) => idToShard(id)
    case RemoteSignal(_, id, _, _) => idToShard(id)
  }

  def extractEntityId: ExtractEntityId = {
    case msg@SubscribeGridInfo(id) =>
      (id.value.toString, msg)
    case msg@NeighboursInitialized(id, _, _, _) =>
      (id.value.toString, msg)
    case msg@RemotePlans(_, to, _, _) =>
      (to.value.toString, msg)
    case msg@RemoteConsequences(_, to, _, _) =>
      (to.value.toString, msg)
    case msg@RemoteSignal(_, to, _, _) =>
      (to.value.toString, msg)
  }
}