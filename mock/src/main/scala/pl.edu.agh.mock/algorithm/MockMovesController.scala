package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Obstacle, _}

import scala.collection.immutable.TreeSet
import scala.util.Random
import scala.math._

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  override def initialGrid(workerId: WorkerId): (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone,workerId = workerId)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal, destinationPoint = POIFactory.generatePOI(grid),workerId = grid.workerId)

    val metrics = MockMetrics.empty()
    (grid, metrics)
  }



  def calculateNextStep(cell: MockCell, x: Int, y: Int): (Int, Int) = {
    def isInDestinationWorker(cell: MockCell): Boolean = {
      cell.destinationPoint.workerId.value.equals(cell.workerId.value)
    }

    def calculateDirection(current: Int, destination: Int) : Int = {
      destination - current match {
        case z if z<0 => -1
        case z if z==0 => 0
        case _ => 1
      }
    }

    def makeMoveInsideWorker(cell: MockCell): (Int, Int) = {
      val xDirection = calculateDirection(x, cell.destinationPoint.x)
      val yDirection = calculateDirection(y, cell.destinationPoint.y)
      (x + xDirection, y + yDirection)
    }

    def makeMoveToAnotherWorker(cell: MockCell): (Int, Int) = {
      print("Worker dest: ")
      print(cell.destinationPoint.workerId.value)
      print(" worker curr: ")
      println(cell.workerId.value)


      val currentColumn = ((cell.workerId.value - 1) % config.workersRoot) + 1
      val currentRow = ((cell.workerId.value - 1) / config.workersRoot) + 1

      val destinationColumn = ((cell.destinationPoint.workerId.value - 1) % config.workersRoot) + 1
      val destinationRow = ((cell.destinationPoint.workerId.value - 1) / config.workersRoot) + 1

      val xDirection = calculateDirection(currentRow, destinationRow)
      val yDirection = calculateDirection(currentColumn, destinationColumn)

      print("Current: row ")
      print(currentRow)
      print("column ")
      println(currentColumn)
      print("Dest: row ")
      print(destinationRow)
      print("column ")
      println(destinationColumn)

      print("dir x: ")
      print(xDirection)
      print("dir y: ")
      print(yDirection)

      println()

      (x + xDirection, y + yDirection)
    }

    if (isInDestinationWorker(cell)) {
      makeMoveInsideWorker(cell)
    } else {
      makeMoveToAnotherWorker(cell)
    }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone,workerId = grid.workerId)
    Thread.sleep(300)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {

      def makeMockMove(occupiedCell: MockCell): Unit = {
        if (occupiedCell.destinationPoint == Point(x,y,occupiedCell.workerId) || !isDestinationPointAccessible(grid,occupiedCell) ) {
          occupiedCell.destinationPoint = POIFactory.generatePOI(grid)
        }

        val destination = calculateNextStep(occupiedCell, x, y)
        val vacatedCell = EmptyCell(cell.smell)
        newGrid.cells(destination._1)(destination._2) match {
          case EmptyCell(_) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _,_) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = occupiedCell

          case BufferCell(EmptyCell(_)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _,_) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)

          case BufferCell(MockCell(_, anotherCrowd, _,_)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _,_) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = BufferCell(MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
              occupiedCell.crowd + anotherCrowd, occupiedCell.destinationPoint ,grid.workerId))

          case MockCell(_, anotherCrowd, _,_) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _,_) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) = MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
              occupiedCell.crowd + anotherCrowd, occupiedCell.destinationPoint,grid.workerId)

          case Obstacle =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case MockCell(_, anotherCrowd, _, _) => MockCell.create(config.mockInitialSignal * (occupiedCell.crowd + anotherCrowd),
                occupiedCell.crowd + anotherCrowd, occupiedCell.destinationPoint,grid.workerId)
              case _ => occupiedCell
            }

          case _ =>
            throw new UnsupportedOperationException(s"Unresolved move, wtf bro?")
        }
      }

      var crowd = cell.asInstanceOf[MockCell].crowd
    //TODO save destination point when creating crowd
      if (crowd > 1) {
        crowd -= 1
        val child = MockCell.create(config.mockInitialSignal, destinationPoint = cell.asInstanceOf[MockCell].destinationPoint,workerId = grid.workerId)
        makeMockMove(child)
      }

      val occupiedCell = MockCell.create(config.mockInitialSignal * crowd, crowd, cell.asInstanceOf[MockCell].destinationPoint,workerId = grid.workerId)

      makeMockMove(occupiedCell)

    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockCell(_, _, _,_)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    val mockPopulation = dynamicCells.foldLeft(0)({ (acc, n) => acc + n._3.asInstanceOf[MockCell].crowd })
    val metrics = MockMetrics(mockPopulation)
    (newGrid, metrics)
  }

  def isDestinationPointAccessible(grid: Grid, cell: MockCell): Boolean = {
    val point = cell.destinationPoint
    if (point.workerId.value != cell.workerId.value) return true;
    grid.cells(point.x)(point.y) match {
      case Obstacle => false
      case _ => true
    }
  }

}