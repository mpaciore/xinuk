package pl.edu.agh.torch.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model._
import pl.edu.agh.torch.simulation.TorchMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class TorchMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: TorchConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, TorchMetrics) = {
    val grid = Grid.empty(bufferZone)
    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grid.cells(x)(y) =
          random.nextInt(3) match {
            case 0 =>
              if (random.nextDouble() < config.humanSpawnChance) {
                humanCount += 1
                val speed = random.nextInt(config.humanMaxSpeed) + 1
                HumanAccessible.unapply(EmptyCell.Instance).withHuman(List.empty, speed)
              } else {
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.escapeSpawnChance) {
                escapesCount += 1
                EscapeAccessible.unapply(EmptyCell.Instance).withEscape()
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (random.nextDouble() < config.fireSpawnChance) {
                fireCount += 1
                FireAccessible.unapply(EmptyCell.Instance).withFire()
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }

    val metrics = TorchMetrics(humanCount, fireCount, escapesCount, 0, 0)
    (grid, metrics)
  }


  def calculatePossibleDestinations(cell: HumanCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map {
        case (i, j) => cell.smell(i)(j)
      }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map {
        case (_, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map {
        case (i, j, current) => (i, j, current, newGrid.cells(i)(j))
      }
      .collectFirstOpt {
        case (i, j, currentCell@HumanAccessible(_), HumanAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, TorchMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    var peopleDeaths = 0L
    var peopleEscaped = 0L

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val availableCells =
        Grid.neighbourCellCoordinates(x, y).flatMap {
          case (i, j) =>
            grid.cells(i)(j).opt
              .filter(_ => creator.isDefinedAt(newGrid.cells(i)(j))) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (availableCells.nonEmpty) {
        val (newFireX, newFireY, newCell) = availableCells(random.nextInt(availableCells.size))
        newGrid.cells(newFireX)(newFireY) = newCell
        grid.cells(newFireX)(newFireY) match {
          case HumanCell(_, _, _) =>
            peopleDeaths += 1
          case _ =>
        }
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case EscapeCell(_) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = EscapeAccessible.unapply(EmptyCell.Instance).withEscape()
          }
        case cell: FireCell =>
          if (iteration % config.fireSpeadingFrequency == 0) {
            reproduce(x, y) {
              case FireAccessible(accessible) => accessible.withFire()
            }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell.copy()
          }
        case cell: HumanCell =>
          newGrid.cells(x)(y) match {
            case FireCell(_) =>
            case _ => if (iteration % cell.speed == 0) {
              moveHuman(cell, x, y)
            } else {
              stayInPlace(cell, x, y)
            }
          }
      }
    }

    def stayInPlace(cell: HumanCell, x: Int, y: Int): Unit = {
      newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
      grid.cells(x)(y)
    }

    def moveHuman(cell: HumanCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      if (cell.crowd.isEmpty) {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.cells(i)(j) = destination.withHuman(cell.crowd, cell.speed)
            newGrid.cells(i)(j) match {
              case EscapeCell(_) => peopleEscaped += 1
              case _ =>
            }
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
        }
      } else {
        destination match {
          case Opt((i, j, HumanAccessible(destination))) =>
            newGrid.cells(i)(j) = destination.withHuman(cell.crowd.head.crowd, cell.crowd.head.speed)
            newGrid.cells(x)(y) = cell.copy(cell.smellWithoutArray(cell.crowd.head.smell), cell.crowd.drop(1), cell.speed)
          case Opt((i, j, inaccessibleDestination)) =>
            throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
          case Opt.Empty =>
            newGrid.cells(x)(y) = cell.copy(cell.smell, cell.crowd, cell.speed)
        }
      }

    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      grid.cells(x)(y) match {
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
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = TorchMetrics(humanCount, fireCount, escapesCount, peopleDeaths, peopleEscaped)
    (newGrid, metrics)
  }
}