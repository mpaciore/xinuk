package pl.edu.agh.formin

import akka.actor.{Actor, Props, Stash}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import com.avsystem.commons.SharedExtensions._
import org.slf4j.{Logger, LoggerFactory, MarkerFactory}
import pl.edu.agh.formin.WorkerActor._
import pl.edu.agh.formin.algorithm.{Metrics, MovesController}
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.Neighbour

import scala.collection.immutable.TreeSet
import scala.collection.mutable

class WorkerActor private(implicit config: ForminConfig) extends Actor with Stash {

  private var id: WorkerId = _

  private var grid: Grid = _

  private var neighbours: Map[WorkerId, Neighbour] = _

  private val finished: mutable.Map[Long, Vector[IncomingNeighbourCells]] = mutable.Map.empty.withDefaultValue(Vector.empty)

  private var bufferZone: TreeSet[(Int, Int)] = _

  private var logger: Logger = _

  private var movesController: MovesController = _

  override def receive: Receive = stopped

  private def propagateSignal(): Unit = {
    (0 until config.signalSpeedRatio).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        grid.propagatedSignal(x, y)
      )
      grid = Grid(cells)
    }
  }

  def stopped: Receive = {
    case NeighboursInitialized(id, neighbours) =>
      this.id = id
      this.logger = LoggerFactory.getLogger(id.value.toString)
      this.neighbours = neighbours.mkMap(_.position.neighbourId(id).get, identity)
      logger.info(s"${id.value} neighbours: ${neighbours.map(_.position).toList}")
      bufferZone = neighbours.foldLeft(TreeSet.empty[(Int, Int)])((builder, neighbour) => builder | neighbour.position.bufferZone)
      grid = Grid.empty(bufferZone)
      movesController = new MovesController(bufferZone, logger)
      self ! StartIteration(1)
    case StartIteration(1) =>
      grid = movesController.initializeGrid()
      propagateSignal()
      notifyNeighbours(1, grid)
      unstashAll()
      context.become(started)
    case _: IterationPartFinished =>
      stash()
  }

  var currentIteration: Long = 1

  def started: Receive = {
    case StartIteration(i) =>
      finished.remove(i - 1)
      logger.debug(s"$id started $i")
      propagateSignal()
      grid = movesController.makeMoves(i, grid)
      notifyNeighbours(i, grid)
      if (i % 100 == 0) logger.info(s"$id finished $i")
    case IterationPartFinished(workerId, _, iteration, neighbourBuffer) =>
      val currentlyFinished: Vector[IncomingNeighbourCells] = finished(iteration)
      val incomingNeighbourCells: IncomingNeighbourCells =
        if (workerId != id) {
          val neighbour = neighbours(workerId)
          val affectedCells: Iterator[(Int, Int)] = neighbour.position.affectedCells
          val incoming: Vector[((Int, Int), BufferCell)] =
            affectedCells.zip(neighbourBuffer.iterator)
              .filterNot { case ((x, y), _) => bufferZone.contains((x, y)) } //at most 8 cells are discarded
              .toVector

          new IncomingNeighbourCells(incoming)
        } else {
          new IncomingNeighbourCells(Vector.empty)
        }
      finished(iteration) = currentlyFinished :+ incomingNeighbourCells
      if (config.iterationsNumber > currentIteration) {
        val incomingCells = finished(currentIteration)
        if (incomingCells.size == neighbours.size + 1) {
          //todo configurable strategy
          incomingCells.foreach(_.cells.foreach {
            case ((x, y), BufferCell(cell)) =>
              val currentCell = grid.cells(x)(y).asInstanceOf[Cell]
              grid.cells(x)(y) = ForminConflictResolver.resolveConflict(currentCell, cell)
          })

          //clean buffers
          bufferZone.foreach { case (x, y) =>
            grid.cells(x)(y) = BufferCell(EmptyCell.Instance)
          }

          currentIteration += 1
          self ! StartIteration(currentIteration)
        }
      } else if (finished(currentIteration).size == neighbours.size + 1) {
        import scala.concurrent.duration._
        Thread.sleep(10.seconds.toMillis)
        context.system.terminate()
      }
  }

  private def notifyNeighbours(iteration: Long, grid: Grid): Unit = {
    self ! IterationPartFinished(id, id, iteration, Array.empty)
    neighbours.foreach { case (neighbourId, ngh) =>
      val bufferArray = ngh.position.bufferZone.iterator.map { case (x, y) => grid.cells(x)(y).asInstanceOf[BufferCell] }.toArray
      Simulation.WorkerRegionRef ! IterationPartFinished(id, neighbourId, iteration, bufferArray)
    }
  }
}

object WorkerActor {

  final val Name: String = "WorkerActor"

  final val MetricsMarker = MarkerFactory.getMarker("METRICS")

  private final class IncomingNeighbourCells(val cells: Vector[((Int, Int), BufferCell)]) extends AnyVal

  final case class NeighboursInitialized(id: WorkerId, neighbours: Vector[Neighbour])

  final case class StartIteration private(i: Long) extends AnyVal

  //sent to listeners
  final case class IterationPartFinished private(worker: WorkerId, to: WorkerId, iteration: Long, incomingBuffer: Array[BufferCell])

  final case class IterationPartMetrics private(workerId: WorkerId, iteration: Long, metrics: Metrics)

  def props(implicit config: ForminConfig): Props = {
    Props(new WorkerActor)
  }

  private def idToShard(id: WorkerId): String = (id.value % 144).toString

  def extractShardId: ExtractShardId = {
    case NeighboursInitialized(id, _) => idToShard(id)
    case IterationPartFinished(_, id, _, _) => idToShard(id)
  }

  def extractEntityId: ExtractEntityId = {
    case msg@NeighboursInitialized(id, _) =>
      (id.value.toString, msg)
    case msg@IterationPartFinished(_, to, _, _) =>
      (to.value.toString, msg)
  }
}