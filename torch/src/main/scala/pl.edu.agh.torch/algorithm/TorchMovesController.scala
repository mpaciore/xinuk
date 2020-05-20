package pl.edu.agh.torch.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model._
import pl.edu.agh.torch.simulation.TorchMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.Cell._
import pl.edu.agh.xinuk.model._

import scala.util.Random

final class TorchMovesController(implicit config: TorchConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def makeMoves(iteration: Long, grid: EnhancedGrid): (EnhancedGrid, TorchMetrics) = {
    val newGrid = grid.emptyCopy()

    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    var peopleDeaths = 0L
    var peopleEscaped = 0L

    def isEmptyIn(grid: EnhancedGrid)(i: Int, j: Int): Boolean = {
      grid.getCellAt(i, j).cell match {
        case EmptyCell(_) => true
        case _ => false
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      grid.getCellAt(x, y).cell match {
        case Obstacle =>
          newGrid.setCellAt(x, y, Obstacle)
        case cell: EmptyCell =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y, cell)
          }
        case EscapeCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y, EscapeAccessible.unapply(EmptyCell.Instance).withEscape())
          }
        case cell: FireCell =>
          if (iteration % config.fireSpeadingFrequency == 0) {
            reproduce(x, y) {
              case FireAccessible(accessible) => accessible.withFire()
            }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.setCellAt(x, y, cell.copy())
          }
        case cell: HumanCell =>
          newGrid.getCellAt(x, y).cell match {
            case FireCell(_) =>
            case _ => if (iteration % cell.speed == 0) {
              moveHuman(cell, x, y)
            } else {
              stayInPlace(cell, x, y)
            }
          }
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val availableCells =
        grid.getCellAt(x, y).neighbours.toList.flatMap {
          case (_, (i, j)) =>
            grid.getCellAt(i, j).cell.opt
              .filter(_ => creator.isDefinedAt(newGrid.getCellAt(i, j).cell)) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (availableCells.nonEmpty) {
        val (newFireX, newFireY, newFireCell) = availableCells(random.nextInt(availableCells.size))
        newGrid.setCellAt(newFireX, newFireY, newFireCell)
        grid.getCellAt(newFireX, newFireY).cell match {
          case HumanCell(_, _, _) =>
            peopleDeaths += 1
          case _ =>
        }
      }
    }

    def moveHuman(cell: HumanCell, x: Int, y: Int): Unit = {
      val destination = selectDestinationCell(calculatePossibleDestinations(x, y))
      if (cell.crowd.isEmpty) {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.setCellAt(i, j, destination.withHuman(cell.crowd, cell.speed))
            newGrid.getCellAt(i, j).cell match {
              case EscapeCell(_) => peopleEscaped += 1
              case _ =>
            }
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.setCellAt(x, y, cell.copy(cell.smell, cell.crowd, cell.speed))
        }
      } else {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.setCellAt(i, j, destination.withHuman(cell.crowd.head.crowd, cell.crowd.head.speed))
            newGrid.setCellAt(x, y, cell.copy(cell.smell - cell.crowd.head.smell, cell.crowd.drop(1), cell.speed))
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.setCellAt(x, y, cell.copy(cell.smell, cell.crowd, cell.speed))
        }
      }
    }

    def calculatePossibleDestinations(x: Int, y: Int): Iterator[(Int, Int)] = {
      val enhancedCell = grid.getCellAt(x, y)
      random.shuffle(enhancedCell.cell.smell
        .toList
        .map(_.swap)
        .filter { case (_, direction) => enhancedCell.neighbours.contains(direction) })
        .sortBy(_._1)(Ordering[Signal].reverse)
        .iterator
        .map { case (_, direction) => enhancedCell.neighbours(direction) }
    }

    def selectDestinationCell(possibleDestinations: Iterator[(Int, Int)]): commons.Opt[(Int, Int, GridPart)] = {
      possibleDestinations
        .map {
          case (i, j) => (i, j, grid.getCellAt(i, j).cell, newGrid.getCellAt(i, j).cell)
        }
        .collectFirstOpt {
          case (i, j, currentCell@HumanAccessible(_), HumanAccessible(_)) => (i, j, currentCell)
        }
    }

    def stayInPlace(cell: HumanCell, x: Int, y: Int): Unit = {
      newGrid.setCellAt(x, y, cell.copy(cell.smell, cell.crowd, cell.speed))
    }

    for {
      x <- grid.xRange
      y <- grid.yRange
    } {
      grid.getCellAt(x, y).cell match {
        case HumanCell(_, crowd, _) =>
          humanCount += 1 + crowd.size
        case FireCell(_) =>
          fireCount += 1
        case EscapeCell(_) =>
          escapesCount += 1
        case _ =>
      }
    }

    for {
      x <- grid.xRange
      y <- grid.yRange
    } makeMove(x, y)

    val metrics = TorchMetrics(humanCount, fireCount, escapesCount, peopleDeaths, peopleEscaped)
    (newGrid, metrics)
  }
}

object TorchMovesController {
  def apply(implicit config: TorchConfig): TorchMovesController = new TorchMovesController
}