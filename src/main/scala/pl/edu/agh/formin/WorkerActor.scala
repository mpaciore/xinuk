package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, Props}
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.formin.SchedulerActor.IterationPartFinished
import pl.edu.agh.formin.WorkerActor.StartIteration
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._

import scala.util.Random

class WorkerActor private(id: WorkerId)(implicit config: ForminConfig) extends Actor with ActorLogging {

  private var grid = Grid.empty

  private val random = new Random(System.nanoTime())

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
    val newGrid = Grid.empty

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) => true
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[Cell, Cell]): Unit = {
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
        case cell: EmptyCell =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: AlgaeCell =>
          if (iteration % config.algaeReproductionFrequency == 0) {
            reproduce(x, y) { case empty: EmptyCell => empty.withAlgae }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: ForaminiferaCell =>
          if (cell.energy < config.foraminiferaLifeActivityCost) {
            newGrid.cells(x)(y) = EmptyCell(cell.smell)
          } else if (cell.energy > config.foraminiferaReproductionThreshold) {
            reproduce(x, y) { case accessible: ForaminiferaAcessible => accessible.withForaminifera(config.foraminiferaStartEnergy) }
            newGrid.cells(x)(y) = cell.copy(energy = cell.energy - config.foraminiferaReproductionCost)
          } else {
            val neighbourCoordinates = Grid.neighbourCoordinates(x, y)
            val destinationCoords =
              Grid.SubcellCoordinates
                .map { case (i, j) => cell.smell(i)(j) }
                .zipWithIndex
                .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
                .map { case (_, idx) => neighbourCoordinates(idx) }

            destinationCoords
             .iterator
              .map { case (i, j) => (i, j, grid.cells(i)(j)) }
              .collectFirstOpt { case (i, j, destination: ForaminiferaAcessible) => (i, j, destination) } match {
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

  def stopped: Receive = {
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
      sender() ! IterationPartFinished(1, SimulationStatus(id, grid))
      context.become(started)
  }

  def started: Receive = {
    case StartIteration(i) =>
      propagateSignal()
      makeMoves(i)
      sender() ! IterationPartFinished(i, SimulationStatus(id, grid))
  }
}

object WorkerActor {

  case class StartIteration(i: Long) extends AnyVal

  def props(id: WorkerId)(implicit config: ForminConfig): Props = {
    Props(new WorkerActor(id))
  }
}


case class WorkerId(value: Int) extends AnyVal