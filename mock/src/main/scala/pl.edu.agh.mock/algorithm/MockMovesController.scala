package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Obstacle, _}

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal)

    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone)
    Thread.sleep(100)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      val destination = (x + random.nextInt(3) - 1, y + random.nextInt(3) - 1)
      val vacatedCell = EmptyCell(cell.smell)
      val crowd = cell.asInstanceOf[MockCell].crowd
      val occupiedCell = MockCell.create(config.mockInitialSignal * crowd, crowd)

      newGrid.cells(destination._1)(destination._2) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
            case occupied@MockCell(_, _) => occupied
            case _ => vacatedCell
          }
          newGrid.cells(destination._1)(destination._2) = occupiedCell

        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)

        case BufferCell(MockCell(_, anotherCrowd)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
            occupiedCell.crowd + anotherCrowd))

        case MockCell(_, anotherCrowd) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
            occupiedCell.crowd + anotherCrowd)

        case Obstacle =>
          newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
            case MockCell(_, anotherCrowd) => MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
              occupiedCell.crowd + anotherCrowd)
            case _ => occupiedCell
          }

        case _ =>
          throw new UnsupportedOperationException(s"Unresolved move, wtf bro?")
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockCell(_, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    val mockPopulation = dynamicCells.foldLeft(0)({ (acc, n) => acc + n._3.asInstanceOf[MockCell].crowd })
    val metrics = MockMetrics(mockPopulation)
    (newGrid, metrics)
  }
}