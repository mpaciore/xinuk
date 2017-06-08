package pl.edu.agh.formin

import akka.actor.{Actor, Props, Stash}
import akka.cluster.sharding.ShardRegion.{ExtractEntityId, ExtractShardId}
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import org.slf4j.{Logger, LoggerFactory, MarkerFactory}
import pl.edu.agh.formin.WorkerActor._
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.model.parallel.{DefaultConflictResolver, Neighbour}

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.util.Random

class WorkerActor private(implicit config: ForminConfig) extends Actor with Stash {

  private var id: WorkerId = _

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  private var neighbours: Map[WorkerId, Neighbour] = _

  private val finished: mutable.Map[Long, Vector[IncomingNeighbourCells]] = mutable.Map.empty.withDefaultValue(Vector.empty)

  private var bufferZone: TreeSet[(Int, Int)] = _

  private var logger: Logger = _

  override def receive: Receive = stopped

  private def propagateSignal(): Unit = {
    (0 until config.signalSpeedRatio).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        grid.propagatedSignal(x, y)
      )
      grid = Grid(cells)
    }
  }

  /**
    * @return (foraminiferaCount, algaeCount)
    */
  private def makeMoves(iteration: Long): Metrics = {
    val newGrid = Grid.empty(bufferZone)

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val emptyCells =
        Grid.neighbourCellCoordinates(x, y).flatMap {
          case (i, j) =>
            grid.cells(i)(j).opt
              .filter(_ => creator.isDefinedAt(newGrid.cells(i)(j))) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (emptyCells.nonEmpty) {
        val (newAlgaeX, newAlgaeY, newCell) = emptyCells(random.nextInt(emptyCells.size))
        newGrid.cells(newAlgaeX)(newAlgaeY) = newCell
      }
    }

    var foraminiferaCount = 0L
    var algaeCount = 0L
    var foraminiferaDeaths = 0L
    var foraminiferaReproductionsCount = 0L
    var consumedAlgaeCount = 0L
    var foraminiferaTotalLifespan = 0L
    var algaeTotalLifespan = 0L
    var foraminiferaTotalEnergy = 0.0

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: AlgaeCell =>
          if (iteration % config.algaeReproductionFrequency == 0) {
            reproduce(x, y) { case accessible: AlgaeAccessible => accessible.withAlgae(0) }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell.copy(lifespan = cell.lifespan + 1)
          }
        case cell: ForaminiferaCell =>
          if (cell.energy < config.foraminiferaLifeActivityCost) {
            foraminiferaDeaths += 1
            foraminiferaTotalLifespan += cell.lifespan
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          } else if (cell.energy > config.foraminiferaReproductionThreshold) {
            reproduce(x, y) { case accessible: ForaminiferaAccessible => accessible.withForaminifera(config.foraminiferaStartEnergy, 0) }
            newGrid.cells(x)(y) = cell.copy(energy = cell.energy - config.foraminiferaReproductionCost, lifespan = cell.lifespan + 1)
            foraminiferaReproductionsCount += 1
          } else {
            //moving
            val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
            val destinations =
              Grid.SubcellCoordinates
                .map { case (i, j) => cell.smell(i)(j) }
                .zipWithIndex
                .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
                .iterator
                .map { case (_, idx) =>
                  val (i, j) = neighbourCellCoordinates(idx)
                  (i, j, grid.cells(i)(j))
                }

            destinations
              .collectFirstOpt {
                case (i, j, destination: AlgaeCell) =>
                  consumedAlgaeCount += 1
                  algaeTotalLifespan += destination.lifespan
                  (i, j, destination)
                case (i, j, destination: EmptyCell) =>
                  val effectiveDestination = newGrid.cells(i)(j) match {
                    case newAlgae: AlgaeCell =>
                      consumedAlgaeCount += 1
                      algaeTotalLifespan += newAlgae.lifespan
                      newAlgae
                    case _ => destination
                  }
                  (i, j, effectiveDestination)
              } match {
              case Opt((i, j, destinationCell)) =>
                newGrid.cells(i)(j) = destinationCell.withForaminifera(cell.energy - config.foraminiferaLifeActivityCost, cell.lifespan + 1)
                newGrid.cells(x)(y) = EmptyCell(cell.smell)
              case Opt.Empty =>
                newGrid.cells(x)(y) = cell.copy(cell.energy - config.foraminiferaLifeActivityCost, lifespan = cell.lifespan + 1)
            }
          }
      }
      newGrid.cells(x)(y) match {
        case ForaminiferaCell(energy, _, _) =>
          foraminiferaTotalEnergy += energy.value
          foraminiferaCount += 1
        case BufferCell(ForaminiferaCell(energy, _, _)) =>
          foraminiferaTotalEnergy += energy.value
          foraminiferaCount += 1
        case AlgaeCell(_, _) | BufferCell(AlgaeCell(_, _)) =>
          algaeCount += 1
        case _ =>
      }
    }
    grid = newGrid
    val metrics = Metrics(foraminiferaCount, algaeCount, foraminiferaDeaths, foraminiferaTotalEnergy, foraminiferaReproductionsCount, consumedAlgaeCount, foraminiferaTotalLifespan, algaeTotalLifespan)
    logMetrics(iteration, metrics)
    metrics
  }

  private def logMetrics(iteration: Long, metrics: Metrics): Unit = {
    logger.info(MetricsMarker, "{};{}", iteration, metrics)
  }

  def stopped: Receive = {
    case NeighboursInitialized(id, neighbours) =>
      this.id = id
      this.logger = LoggerFactory.getLogger(id.value.toString)
      this.neighbours = neighbours.mkMap(_.position.neighbourId(id).get, identity)
      logger.info(s"${id.value} neighbours: ${neighbours.map(_.position).toList}")
      bufferZone = neighbours.foldLeft(TreeSet.empty[(Int, Int)])((builder, neighbour) => builder | neighbour.position.bufferZone)
      grid = Grid.empty(bufferZone)
      self ! StartIteration(1)
    case StartIteration(1) =>
      var foraminiferaCount = 0L
      var algaeCount = 0L
      for {
        x <- 0 until config.gridSize
        y <- 0 until config.gridSize
        if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
      } {
        if (random.nextDouble() < config.spawnChance) {
          grid.cells(x)(y) =
            if (random.nextDouble() < config.foraminiferaSpawnChance) {
              foraminiferaCount += 1
              EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
            }
            else {
              algaeCount += 1
              EmptyCell.Instance.withAlgae(0)
            }
        }
      }
      propagateSignal()
      val metrics = Metrics(foraminiferaCount, algaeCount, 0, config.foraminiferaStartEnergy.value * foraminiferaCount, 0, 0, 0, 0)
      logMetrics(1, metrics)
      notifyNeighbours(1, grid, metrics)
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
      val metrics = makeMoves(i)
      notifyNeighbours(i, grid, metrics)
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
              grid.cells(x)(y) = DefaultConflictResolver.resolveConflict(currentCell, cell)
          })

          //clean buffers
          bufferZone.foreach { case (x, y) =>
            grid.cells(x)(y) = BufferCell(EmptyCell.Instance)
          }

          currentIteration += 1
          self ! StartIteration(currentIteration)
        }
      } else if (finished(currentIteration).size == neighbours.size + 1) {
        context.system.terminate()
      }
  }

  private def notifyNeighbours(iteration: Long, grid: Grid, metrics: Metrics): Unit = {
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

  private def idToShard(id: WorkerId): String = (id.value % 2).toString

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

final case class Metrics(foraminiferaCount: Long,
                         algaeCount: Long,
                         foraminiferaDeaths: Long,
                         foraminiferaTotalEnergy: Double,
                         foraminiferaReproductionsCount: Long,
                         consumedAlgaeCount: Long,
                         foraminiferaTotalLifespan: Long,
                         algaeTotalLifespan: Long) {
  override def toString: String = {
    s"$foraminiferaCount;$algaeCount;$foraminiferaDeaths;$foraminiferaTotalEnergy;$foraminiferaReproductionsCount;$consumedAlgaeCount;$foraminiferaTotalLifespan;$algaeTotalLifespan"
  }
}

final case class WorkerId(value: Int) extends AnyVal {
  def isValid(implicit config: ForminConfig): Boolean = (value > 0) && (value <= math.pow(config.workersRoot, 2))
}

object WorkerId {
  implicit val WorkerOrdering: Ordering[WorkerId] = Ordering.by(_.value)
}