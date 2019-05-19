package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal.toSignalVector)

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
      val occupiedCell = MockCell.create(config.mockInitialSignal.toSignalVector)

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
      case (_, _, MockCell(_)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}