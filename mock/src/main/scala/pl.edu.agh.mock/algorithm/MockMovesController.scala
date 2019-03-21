package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

    def inBox(distance: Int, topClosed: Boolean, bottomClosed: Boolean, leftClosed: Boolean, rightClosed: Boolean)(x: Int, y: Int): Boolean = {
      val altDistance = config.gridSize - distance
      (x == distance && y >= distance && y <= altDistance && topClosed) ||
        (x == altDistance && y >= distance && y <= altDistance && bottomClosed) ||
        (y == distance && x >= distance && x <= altDistance && leftClosed) ||
        (y == altDistance && x >= distance && x <= altDistance && rightClosed)
    }

    val gridSizeQuarter = config.gridSize / 4
    for {
      x <- grid.cells.indices
      y <- grid.cells.indices
    } if (inBox(gridSizeQuarter + 5, topClosed = true, bottomClosed = false, leftClosed = true, rightClosed = true)(x, y)
      || inBox(gridSizeQuarter, topClosed = false, bottomClosed = true, leftClosed = true, rightClosed = true)(x, y)
      || inBox(gridSizeQuarter - 5, topClosed = true, bottomClosed = false, leftClosed = true, rightClosed = true)(x, y)) {
      grid.cells(x)(y) = Obstacle
    }
    if (random.nextInt(4) == 0) grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockNonEmptyCell.create(config.mockInitialSignal)

    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      val destination = (x + random.nextInt(3) - 1, y + random.nextInt(3) - 1)
      val vacatedCell = EmptyCell(cell.smell)
      val occupiedCell = MockNonEmptyCell.create(config.mockInitialSignal)

      newGrid.cells(destination._1)(destination._2) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = occupiedCell
        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockNonEmptyCell(_)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}