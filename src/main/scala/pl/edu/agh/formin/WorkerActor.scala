package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.formin.WorkerActor._
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.model.parallel.{DefaultConflictResolver, Neighbour}

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.util.Random

class WorkerActor private(id: WorkerId)(implicit config: ForminConfig) extends Actor with ActorLogging with Stash {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  private val gridListeners: mutable.Set[ActorRef] = mutable.Set.empty
  private val metricsListeners: mutable.Set[ActorRef] = mutable.Set.empty
  private val statusRequests: mutable.Set[ActorRef] = mutable.Set.empty

  private var neighbours: Map[WorkerId, Neighbour] = _

  private val finished: mutable.Map[Long, Vector[IncomingNeighbourCells]] = mutable.Map.empty.withDefaultValue(Vector.empty)

  private var bufferZone: TreeSet[(Int, Int)] = _

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
            reproduce(x, y) { case accessible: AlgaeAccessible => accessible.withAlgae }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: ForaminiferaCell =>
          if (cell.energy < config.foraminiferaLifeActivityCost) {
            foraminiferaDeaths+=1
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          } else if (cell.energy > config.foraminiferaReproductionThreshold) {
            reproduce(x, y) { case accessible: ForaminiferaAccessible => accessible.withForaminifera(config.foraminiferaStartEnergy) }
            newGrid.cells(x)(y) = cell.copy(energy = cell.energy - config.foraminiferaReproductionCost)
            foraminiferaReproductionsCount+=1
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
                case (i, j, destination: AlgaeCell) => (i, j, destination)
                case (i, j, destination: EmptyCell) =>
                  val effectiveDestination = newGrid.cells(i)(j) match {
                    case newAlgae: AlgaeCell => newAlgae
                    case _ => destination
                  }
                  (i, j, effectiveDestination)
              } match {
              case Opt((i, j, destinationCell)) =>
                newGrid.cells(i)(j) = destinationCell.withForaminifera(cell.energy - config.foraminiferaLifeActivityCost)
                newGrid.cells(x)(y) = EmptyCell(cell.smell)
              case Opt.Empty =>
                newGrid.cells(x)(y) = cell.copy(cell.energy - config.foraminiferaLifeActivityCost)
            }
          }
      }
      newGrid.cells(x)(y) match {
        case ForaminiferaCell(energy, _) =>
          foraminiferaTotalEnergy+=energy.value
          foraminiferaCount += 1
        case BufferCell(ForaminiferaCell(energy, _)) =>
          foraminiferaTotalEnergy+=energy.value
          foraminiferaCount += 1
        case AlgaeCell(_) | BufferCell(AlgaeCell(_)) =>
          algaeCount += 1
        case _ =>
      }
    }
    grid = newGrid
    Metrics(foraminiferaCount, algaeCount, foraminiferaDeaths, foraminiferaTotalEnergy, foraminiferaReproductionsCount)
  }

  private def handleRegistrations: Receive = {
    case RegisterGrid =>
      gridListeners += sender
    case DeregisterGrid =>
      gridListeners -= sender
    case RegisterMetrics =>
      metricsListeners += sender
    case DeregisterMetrics =>
      metricsListeners -= sender
    case GetStatus =>
      statusRequests += sender
  }

  def stopped: Receive = {
    val specific: Receive = {
      case NeighboursInitialized(neighbours: Set[Neighbour]) =>
        log.info(s"$id neighbours: ${neighbours.map(_.position).toList}")
        this.neighbours = neighbours.mkMap(_.position.neighbourId(id).get, identity)
        gridListeners ++= neighbours.map(_.ref)
        gridListeners += self
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
                EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy)
              }
              else {
                algaeCount += 1
                EmptyCell.Instance.withAlgae
              }
          }
        }
        propagateSignal()
        notifyListeners(1, grid, Metrics(foraminiferaCount, algaeCount, 0, config.foraminiferaStartEnergy.value*foraminiferaCount, 0))
        unstashAll()
        context.become(started)
      case IterationPartFinished(_,_, _) =>
        stash()
    }
    specific.orElse(handleRegistrations)
  }

  var currentIteration: Long = 1

  def started: Receive = {
    val specific: Receive = {
      case StartIteration(i) =>
        finished.remove(i - 1)
        log.debug(s"$id started $i")
        propagateSignal()
        val metrics = makeMoves(i)
        notifyListeners(i, grid, metrics)
        if (i % 100 == 0) log.info(s"$id finished $i")
      case IterationPartFinished(workerId, iteration, incomingGrid) =>
        val currentlyFinished: Vector[IncomingNeighbourCells] = finished(iteration)
        val incomingNeighbourCells: IncomingNeighbourCells =
          if (workerId != id) {
            val neighbour = neighbours(workerId)
            val affectedCells: Iterator[(Int, Int)] = neighbour.position.affectedCells
            val neighbourBuffer: Iterator[BufferCell] =
              neighbour.position.neighbourBuffer.iterator.map { case (x, y) => incomingGrid.cells(x)(y).asInstanceOf[BufferCell] }
            val incoming: Vector[((Int, Int), BufferCell)] =
              affectedCells.zip(neighbourBuffer)
                .filterNot { case ((x, y), _) => bufferZone.contains((x, y)) } //at most 8 cells are discarded
                .toVector

            new IncomingNeighbourCells(incoming)
          } else {
            new IncomingNeighbourCells(Vector.empty)
          }
        finished(iteration) = currentlyFinished :+ incomingNeighbourCells
        if (config.iterationsNumber > currentIteration && iteration == currentIteration) {
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
        }
    }
    specific.orElse(handleRegistrations)
  }

  private def notifyListeners(iteration: Long, grid: Grid, metrics: Metrics): Unit = {
    val partFinished = IterationPartFinished(id, iteration, grid)
    gridListeners.foreach(_ ! partFinished)
    statusRequests.foreach(_ ! partFinished)
    statusRequests.clear()
    metricsListeners.foreach(_ ! IterationPartMetrics(id, iteration, metrics))
  }
}

object WorkerActor {

  private final class IncomingNeighbourCells(val cells: Vector[((Int, Int), BufferCell)]) extends AnyVal

  final case class NeighboursInitialized(neighbours: Set[Neighbour]) extends AnyVal

  final case class StartIteration private(i: Long) extends AnyVal

  case object RegisterGrid

  case object DeregisterGrid

  case object RegisterMetrics

  case object DeregisterMetrics

  case object GetStatus

  //sent to listeners
  final case class IterationPartFinished private(worker: WorkerId, iteration: Long, grid: Grid)

  final case class IterationPartMetrics private(workerId: WorkerId, iteration: Long, metrics: Metrics)

  def props(id: WorkerId)(implicit config: ForminConfig): Props = {
    Props(new WorkerActor(id))
  }
}

final case class Metrics(foraminiferaCount: Long, algaeCount: Long, foraminiferaDeaths: Long, foraminiferaTotalEnergy: Double, foraminiferaReproductionsCount: Long)

final case class WorkerId(value: Int) extends AnyVal {
  def isValid(implicit config: ForminConfig): Boolean = (value > 0) && (value <= math.pow(config.workersRoot, 2))
}

object WorkerId {
  implicit val WorkerOrdering: Ordering[WorkerId] = Ordering.by(_.value)
}