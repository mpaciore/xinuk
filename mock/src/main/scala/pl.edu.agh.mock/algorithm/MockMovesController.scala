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

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal, grid)

    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  def calculateNextStep(cell: MockCell, x: Int, y: Int): (Int, Int) = {
    def calculateDirection(current: Int, destination: Int) : Int = {
      destination - current match {
        case z if z<0 => -1
        case z if z==0 => 0
        case _ => 1
      }
    }

    val xDirection = calculateDirection(x, cell.destinationPoint._1)
    val yDirection = calculateDirection(y, cell.destinationPoint._2)
    (x + xDirection, y + yDirection)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone)
    Thread.sleep(1000)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {

      def makeMockMove(occupiedCell: MockCell): Unit = {
        val destination = calculateNextStep(occupiedCell, x, y)
        val vacatedCell = EmptyCell(cell.smell)
        newGrid.cells(destination._1)(destination._2) match {
          case EmptyCell(_) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = occupiedCell

          case BufferCell(EmptyCell(_)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)

          case BufferCell(MockCell(_, anotherCrowd, _)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = BufferCell(MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
              occupiedCell.crowd + anotherCrowd))

          case MockCell(_, anotherCrowd, _) =>
            newGrid.cells(x)(y) = vacatedCell
            newGrid.cells(destination._1)(destination._2) = MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
              occupiedCell.crowd + anotherCrowd)

          case Obstacle =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case MockCell(_, anotherCrowd, _) => MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
                occupiedCell.crowd + anotherCrowd)
              case _ => occupiedCell
            }

          case _ =>
            throw new UnsupportedOperationException(s"Unresolved move, wtf bro?")
        }
      }

      var crowd = cell.asInstanceOf[MockCell].crowd

      if (crowd > 1) {
        crowd -= 1
        val child = MockCell.create(config.mockInitialSignal)
        makeMockMove(child)
      }

      val occupiedCell = MockCell.create(config.mockInitialSignal * crowd, crowd)

      makeMockMove(occupiedCell)

    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockCell(_, _, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    val mockPopulation = dynamicCells.foldLeft(0)({ (acc, n) => acc + n._3.asInstanceOf[MockCell].crowd })
    val metrics = MockMetrics(mockPopulation)
    (newGrid, metrics)
  }
}