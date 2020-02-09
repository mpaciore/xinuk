package pl.edu.agh.xinuk.simulation

import akka.actor.{Actor, ActorRef, Props, Stash}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import org.slf4j.{Logger, LoggerFactory, MarkerFactory}
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.gui.GuiActor.GridInfo
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

import scala.collection.mutable

class WorkerActor[ConfigType <: XinukConfig](
  regionRef: => ActorRef,
  movesControllerFactory: ConfigType => MovesController,
  conflictResolver: ConflictResolver[ConfigType],
  smellPropagationFunction: (EnhancedGrid, Map[Direction, (Int, Int)]) => SmellMap,
  emptyCellFactory: => SmellingCell = EmptyCell.Instance)(implicit config: ConfigType
) extends Actor with Stash {

  import pl.edu.agh.xinuk.simulation.WorkerActor._

  var grid: EnhancedGrid = _

  var id: WorkerId = _

  val guiActors: mutable.Set[ActorRef] = mutable.Set.empty

  var outgoingNeighbours: Set[WorkerId] = _

  var incomingNeighbours: Set[WorkerId] = _

  val finished: mutable.Map[Long, Set[Set[((Int, Int), GridPart)]]] = mutable.Map.empty.withDefaultValue(Set.empty)

  var logger: Logger = _

  var movesController: MovesController = _

  var conflictResolutionMetrics: Metrics = _

  var currentIteration: Long = 1

  override def receive: Receive = stopped

  private def propagateSignal(): Unit = {
    (0 until config.signalSpeedRatio).foreach { _ => grid = grid.propagatedSignal(smellPropagationFunction) }
  }
  def stopped: Receive = {
    case SubscribeGridInfo(_) =>
      guiActors += sender()
    case NeighboursInitialized(id, grid, outgoingNeighbours, incomingNeighbours) =>
      this.id = id
      this.grid = grid
      this.outgoingNeighbours = outgoingNeighbours
      this.incomingNeighbours = incomingNeighbours
      this.movesController = movesControllerFactory(config)
      this.logger = LoggerFactory.getLogger(id.value.toString)
      logger.info(s"${id.value} " +
        s"outgoing neighbours: ${outgoingNeighbours.map(_.value)} " +
        s"incoming neighbours: ${incomingNeighbours.map(_.value)}")
      self ! StartIteration(1)
      unstashAll()
      context.become(started)
    case _: IterationPartFinished =>
      stash()
  }

  def started: Receive = {

    case SubscribeGridInfo(_) =>
      guiActors += sender()

    case StartIteration(iteration) =>
      finished.remove(iteration - 1)
      logger.debug(s"$id started $iteration")
      val (newGrid, newMetrics) = movesController.makeMoves(iteration, grid)
      grid = newGrid
      val metrics = newMetrics + conflictResolutionMetrics
      logMetrics(iteration, metrics)
      guiActors.foreach(_ ! GridInfo(iteration, grid, metrics))
      propagateSignal()
      notifyNeighbours(iteration, grid)
      conflictResolutionMetrics = null
      if (iteration % 100 == 0) logger.info(s"$id finished $iteration")

    case IterationPartFinished(_, _, iteration, incomingCells) =>
      finished(iteration) += incomingCells
      if (finished(currentIteration).size == incomingNeighbours.size + 1) {
        if (currentIteration < config.iterationsNumber) {
          finished(currentIteration).foreach(_.foreach {
            case ((x, y), cell) =>
              val currentCell = grid.getLocalCellAt(x, y)
              val (resolved, partialResolvingMetrics) = conflictResolver.resolveConflict(currentCell.cell, cell)
              conflictResolutionMetrics = partialResolvingMetrics + conflictResolutionMetrics
              grid.setCellAt(x, y, resolved)
          })
          currentIteration += 1
          self ! StartIteration(currentIteration)
        } else {
          logger.info(s"$id terminating")
          import scala.concurrent.duration._
          Thread.sleep(10.seconds.toMillis)
          context.system.terminate()
        }
      }
  }

  private def notifyNeighbours(iteration: Long, grid: EnhancedGrid): Unit = {
    self ! IterationPartFinished(id, id, iteration, Set.empty)
    outgoingNeighbours.foreach { neighbourId =>
      val outgoingCells = grid.outgoingCells(neighbourId)
      regionRef ! IterationPartFinished(id, neighbourId, iteration, outgoingCells)
    }
    grid.clearOutgoingCells()
  }

  private def logMetrics(iteration: Long, metrics: Metrics): Unit = {
    logger.info(WorkerActor.MetricsMarker, "{};{}", iteration.toString, metrics: Any)
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
  final case class IterationPartFinished private(worker: WorkerId, to: WorkerId, iteration: Long, incomingBuffer: Set[((Int, Int), GridPart)])

  final case class IterationPartMetrics private(workerId: WorkerId, iteration: Long, metrics: Metrics)

  def props[ConfigType <: XinukConfig](
                                        regionRef: => ActorRef,
                                        movesControllerFactory: ConfigType => MovesController,
                                        conflictResolver: ConflictResolver[ConfigType],
                                        smellPropagationFunction: (EnhancedGrid, Map[Direction, (Int, Int)]) => SmellMap,
                                        emptyCellFactory: => SmellingCell = EmptyCell.Instance
                                      )(implicit config: ConfigType): Props = {
    Props(new WorkerActor(regionRef, movesControllerFactory, conflictResolver, smellPropagationFunction, emptyCellFactory))
  }

  private def idToShard(id: WorkerId)(implicit config: XinukConfig): String = (id.value % config.shardingMod).toString

  def extractShardId(implicit config: XinukConfig): ExtractShardId = {
    case NeighboursInitialized(id, _, _, _) => idToShard(id)
    case IterationPartFinished(_, id, _, _) => idToShard(id)
    case SubscribeGridInfo(id) => idToShard(id)
  }

  def extractEntityId: ExtractEntityId = {
    case msg@NeighboursInitialized(id, _, _, _) =>
      (id.value.toString, msg)
    case msg@IterationPartFinished(_, to, _, _) =>
      (to.value.toString, msg)
    case msg@SubscribeGridInfo(id) =>
      (id.value.toString, msg)
  }
}