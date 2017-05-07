package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.formin.WorkerActor._
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.model.parallel.Neighbour

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.util.Random

class WorkerActor private(id: WorkerId)(implicit config: ForminConfig) extends Actor with ActorLogging {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  private val listeners: mutable.Set[ActorRef] = mutable.Set.empty

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
  private def makeMoves(iteration: Long): (Long, Long) = {
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
            reproduce(x, y) { case accessible: AlgaeAccessible[_] => accessible.withAlgae }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: ForaminiferaCell =>
          if (cell.energy < config.foraminiferaLifeActivityCost) {
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          } else if (cell.energy > config.foraminiferaReproductionThreshold) {
            reproduce(x, y) { case accessible: ForaminiferaAccessible[_] => accessible.withForaminifera(config.foraminiferaStartEnergy) }
            newGrid.cells(x)(y) = cell.copy(energy = cell.energy - config.foraminiferaReproductionCost)
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
        case ForaminiferaCell(_, _) | BufferCell(ForaminiferaCell(_, _)) =>
          foraminiferaCount += 1
        case AlgaeCell(_) | BufferCell(AlgaeCell(_)) =>
          algaeCount += 1
        case _ =>
      }
    }
    grid = newGrid
    (foraminiferaCount, algaeCount)
  }

  private def handleRegistrations: Receive = {
    case Register =>
      listeners += sender()
    case Deregister =>
      listeners -= sender()
  }

  def stopped: Receive = {
    val specific: Receive = {
      case NeighboursInitialized(neighbours: Set[Neighbour]) =>
        log.info(s"$id neighbours: ${neighbours.map(_.position).toList}")
        this.neighbours = neighbours.mkMap(_.position.neighbourId(id).get, identity)
        listeners ++= neighbours.map(_.ref)
        listeners += self
        bufferZone = neighbours.foldLeft(TreeSet.empty[(Int, Int)])((builder, neighbour) => builder | neighbour.position.bufferZone)
        grid = Grid.empty(bufferZone)
        self ! StartIteration(1)
      case StartIteration(1) =>
        val empty = EmptyCell()
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
                empty.withForaminifera(config.foraminiferaStartEnergy)
              }
              else {
                algaeCount += 1
                empty.withAlgae
              }
          }
        }
        propagateSignal()
        notifyListeners(1, SimulationStatus(grid, foraminiferaCount, algaeCount))
        context.become(started)
    }
    specific.orElse(handleRegistrations)
  }

  var currentIteration: Long = 1

  def started: Receive = {
    val specific: Receive = {
      case StartIteration(i) =>
        finished.remove(i - 1)
        log.info(s"$id started $i")
        propagateSignal()
        val (foraminiferaCount, algaeCount) = makeMoves(i)
        notifyListeners(i, SimulationStatus(grid, foraminiferaCount, algaeCount))
        log.info(s"$id finished $i")
      case IterationPartFinished(workerId, iteration, status) =>
        val currentlyFinished: Vector[IncomingNeighbourCells] = finished(iteration)
        val incomingNeighbourCells: IncomingNeighbourCells =
          if (workerId != id) {
            val neighbour = neighbours(workerId)
            val affectedCells: Iterator[(Int, Int)] = neighbour.position.affectedCells
            val neighbourBuffer: Iterator[GridPart] = neighbour.position.neighbourBuffer.iterator.map { case (x, y) => status.grid.cells(x)(y) }
            val incoming: Vector[((Int, Int), GridPart)] =
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
            //todo apply incoming cells - conflict resolution

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

  private def notifyListeners(iteration: Long, status: SimulationStatus): Unit = {
    listeners.foreach(_ ! IterationPartFinished(id, iteration, status))
  }
}

object WorkerActor {

  private class IncomingNeighbourCells(val cells: Vector[((Int, Int), GridPart)]) extends AnyVal

  final case class NeighboursInitialized(neighbours: Set[Neighbour]) extends AnyVal

  final case class StartIteration private(i: Long) extends AnyVal

  case object Register

  case object WaitForNeighbours

  case object Deregister

  //sent to listeners
  final case class IterationPartFinished private(worker: WorkerId, iteration: Long, simulationStatus: SimulationStatus)

  def props(id: WorkerId)(implicit config: ForminConfig): Props = {
    Props(new WorkerActor(id))
  }
}

final case class SimulationStatus(grid: Grid, foraminiferaCount: Long, algaeCount: Long)

final case class WorkerId(value: Int) extends AnyVal {
  def isValid(implicit config: ForminConfig): Boolean = (value > 0) && (value <= math.pow(config.workersRoot, 2))
}

object WorkerId {
  implicit val WorkerOrdering: Ordering[WorkerId] = Ordering.by(_.value)
}