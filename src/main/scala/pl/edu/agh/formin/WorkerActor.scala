package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.formin.WorkerActor._
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.model.parallel.Neighbour

import scala.collection.mutable
import scala.util.Random

class WorkerActor private(id: WorkerId)(implicit config: ForminConfig) extends Actor with ActorLogging {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  private val registered: mutable.Set[ActorRef] = mutable.Set.empty

  private var neighbours: Set[Neighbour] = _

  private var bufferZone: Set[(Int, Int)] = _

  override def receive: Receive = stopped

  private def propagateSignal(): Unit = {
    (0 until config.signalSpeedRatio).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        grid.propagatedSignal(x, y)
      )
      grid = Grid(cells)
    }
  }

  private def makeMoves(iteration: Long): Unit = {
    val newGrid = Grid.empty(bufferZone)

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val emptyCells =
        Grid.neighbourCoordinates(x, y).flatMap {
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
            val neighbourCoordinates = Grid.neighbourCoordinates(x, y)
            val destinations =
              Grid.SubcellCoordinates
                .map { case (i, j) => cell.smell(i)(j) }
                .zipWithIndex
                .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
                .iterator
                .map { case (_, idx) =>
                  val (i, j) = neighbourCoordinates(idx)
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
    }
    grid = newGrid
  }

  private def handleRegistrations: Receive = {
    case Register =>
      registered += sender()
    case Deregister =>
      registered -= sender()
  }

  def stopped: Receive = {
    val specific: Receive = {
      case NeighboursInitialized(neighbours: Set[Neighbour]) =>
        this.neighbours = neighbours
        bufferZone = neighbours.foldLeft(Set.empty[(Int, Int)])((builder, neighbour) => builder | neighbour.position.bufferZone)
        grid = Grid.empty(bufferZone)
        self ! StartIteration(1)
      case StartIteration(1) =>
        val empty = EmptyCell()
        for {
          x <- 0 until config.gridSize
          y <- 0 until config.gridSize
          if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
        } {
          if (random.nextDouble() < config.spawnChance) {
            grid.cells(x)(y) =
              if (random.nextDouble() < config.foraminiferaSpawnChance) empty.withForaminifera(config.foraminiferaStartEnergy)
              else empty.withAlgae
          }
        }
        propagateSignal()
        notifyListeners(1, SimulationStatus(id, grid))
        context.become(started)
    }
    specific.orElse(handleRegistrations)
  }

  def started: Receive = {
    val specific: Receive = {
      case StartIteration(i) =>
        propagateSignal()
        makeMoves(i)
        notifyListeners(i, SimulationStatus(id, grid))
    }
    specific.orElse(handleRegistrations)
  }

  private def notifyListeners(iteration: Long, status: SimulationStatus): Unit = {
    registered.foreach(_ ! IterationPartFinished(iteration, status))
  }
}

object WorkerActor {

  case class NeighboursInitialized(neighbours: Set[Neighbour]) extends AnyVal

  case class StartIteration private(i: Long) extends AnyVal

  case object Register

  case object Deregister

  //sent to listeners
  case class IterationPartFinished private(iteration: Long, simulationStatus: SimulationStatus)

  def props(id: WorkerId)(implicit config: ForminConfig): Props = {
    Props(new WorkerActor(id))
  }
}


case class WorkerId(value: Int) extends AnyVal {
  def isValid(implicit config: ForminConfig): Boolean = (value >= 0) && (value < math.pow(config.workersRoot, 2))
}

object WorkerId {
  implicit val WorkerOrdering: Ordering[WorkerId] = Ordering.by(_.value)
}