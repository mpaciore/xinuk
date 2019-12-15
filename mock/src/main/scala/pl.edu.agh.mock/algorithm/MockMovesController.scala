package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{LocalEnhancedCell, _}

import scala.util.Random

final class MockMovesController(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def makeMoves(iteration: Long, grid: EnhancedGrid): (EnhancedGrid, MockMetrics) = {
    val newGrid = grid.emptyCopy()

    def copyCells(x: Int, y: Int, cell: EnhancedCell): Unit = {
      newGrid.setCellAt(x, y, cell.cell)
    }

    def moveCells(x: Int, y: Int, cell: EnhancedCell): Unit = {
      val destination = cell.neighbours.values.toVector(random.nextInt(cell.neighbours.size))
      val vacatedCell = EmptyCell(cell.cell.smell)
      val occupiedCell = MockCell.create(config.mockInitialSignal)

      val destinationCell = newGrid.getCellAt(destination._1, destination._2)
      destinationCell.cell match {
        case EmptyCell(_) =>
          newGrid.setCellAt(x, y, vacatedCell)
          newGrid.setCellAt(destination._1, destination._2, occupiedCell)
        case _ =>
          newGrid.setCellAt(x, y, occupiedCell)
      }
    }

    val cells: Seq[(Int, Int, EnhancedCell)] = for {
      x <- grid.xRange
      y <- grid.yRange
    } yield (x, y, grid.getCellAt(x, y))

    val (dynamicCells, staticCells) = cells.partition({
      case (_, _, LocalEnhancedCell(MockCell(_), _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}

object MockMovesController {
  def apply(implicit config: MockConfig): MockMovesController = new MockMovesController
}