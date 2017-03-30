package pl.edu.agh.formin

import akka.actor.{Actor, Props}
import com.avsystem.commons.SharedExtensions._
import pl.edu.agh.formin.SchedulerActor.IterationPartFinished
import pl.edu.agh.formin.WorkerActor.StartIteration
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._

import scala.util.Random

class WorkerActor private(id: WorkerId)(implicit config: ForminConfig) extends Actor {

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

    def emptyCellsAround(x: Int, y: Int): Vector[(Int, Int, EmptyCell)] = {
      Grid.neighbourCoordinates(x, y).flatMap {
        case (i, j) =>
          grid.cells(i)(j).opt.collect { case cell: EmptyCell if isEmptyIn(newGrid)(i, j) =>
            (i, j, cell)
          }
      }
    }

    def reproduce(x: Int, y: Int)(creator: EmptyCell => Cell): Unit = {
      val emptyCells = emptyCellsAround(x, y)
      if (emptyCells.nonEmpty) {
        val (newAlgaeX, newAlgaeY, oldCell) = emptyCells(random.nextInt(emptyCells.size))
        newGrid.cells(newAlgaeX)(newAlgaeY) = creator(oldCell)
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      val current = grid.cells(x)(y)
      current match {
        case Obstacle =>
        case cell: EmptyCell =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: AlgaeCell =>
          if (iteration % config.algaeReproductionFrequency == 0) {
            reproduce(x, y)(_.withAlgae)
          }
          newGrid.cells(x)(y) = cell
        case cell: ForaminiferaCell =>
          if (cell.energy > config.foraminiferaReproductionThreshold) {
            reproduce(x, y)(_.withForaminifera(cell.energy))
          }
          newGrid.cells(x)(y) = cell
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