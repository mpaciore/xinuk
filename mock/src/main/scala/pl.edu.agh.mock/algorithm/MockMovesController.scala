package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.mock.utlis.{DistanceUtils, MovementDirectionUtils, SmellUtils}
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Obstacle, _}

import scala.collection.immutable.TreeSet
import scala.util.Random
import scala.math.min
import scala.math.max

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  var crowdOnProcessor = 0

  private val random = new Random(System.nanoTime())

  override def initialGrid(workerId: WorkerId): (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone,workerId = workerId)
    if (grid.workerId.value == 1)
      grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal, destinationPoint = POIFactory.generatePOI(grid), workerId = grid.workerId)

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
      val currentColumn = ((cell.workerId.value - 1) % config.workersRoot) + 1
      val currentRow = ((cell.workerId.value - 1) / config.workersRoot) + 1

      val destinationColumn = ((cell.destinationPoint.workerId.value - 1) % config.workersRoot) + 1
      val destinationRow = ((cell.destinationPoint.workerId.value - 1) / config.workersRoot) + 1

      val xDirection = calculateDirection(currentRow, destinationRow)
      val yDirection = calculateDirection(currentColumn, destinationColumn)

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
    Thread.sleep(1000)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {

      def makeMockMove(occupiedCell: MockCell): Unit = {
        if (occupiedCell.destinationPoint == LocalPoint(x,y,occupiedCell.workerId) || !isDestinationPointAccessible(grid,occupiedCell) ) {
          occupiedCell.destinationPoint = POIFactory.generatePOI(grid)
        }
        val point = MovementDirectionUtils.calculateDirection(MovementDirectionUtils.calculateMovementCosts(SmellUtils.calculateNeighboursSmell(occupiedCell,x , y, grid),
          DistanceUtils.calculateNeighboursDistances(occupiedCell,x , y, grid)))
        val destination = Tuple2(point.x, point.y)
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

          case BufferCell(another@MockCell(_, anotherCrowd, _,_)) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _,_) => occupied
              case _ => vacatedCell
            }
            val crowd : List[MockCell] =
              anotherCrowd ++
              List(MockCell.create(
                config.mockInitialSignal,
                destinationPoint = another.destinationPoint,
                workerId = another.workerId
              )) ++
              occupiedCell.crowd

            newGrid.cells(destination._1)(destination._2) =
              BufferCell(
                MockCell.create(config.mockInitialSignal * ((occupiedCell.crowd ++ anotherCrowd).size + 2),
                  crowd,
                  occupiedCell.destinationPoint,
                  grid.workerId
                )
              )

            crowdOnProcessor += 1

          case another@MockCell(_, anotherCrowd, _,_) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _,_) => occupied
              case _ => vacatedCell
            }
            val crowd : List[MockCell] =
              anotherCrowd ++
                List(MockCell.create(
                  config.mockInitialSignal,
                  destinationPoint = another.destinationPoint,
                  workerId = another.workerId
                )) ++
                occupiedCell.crowd
            newGrid.cells(destination._1)(destination._2) =
              MockCell.create(
                config.mockInitialSignal * ((occupiedCell.crowd ++ anotherCrowd).size + 2),
                crowd,
                occupiedCell.destinationPoint,
                grid.workerId
              )

            crowdOnProcessor += 1

          case Obstacle =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case another@MockCell(_, anotherCrowd, _, _) =>
                crowdOnProcessor += 1

                val crowd : List[MockCell] =
                  anotherCrowd ++
                    List(MockCell.create(
                      config.mockInitialSignal,
                      destinationPoint = another.destinationPoint,
                      workerId = another.workerId
                    )) ++
                    occupiedCell.crowd
                MockCell.create(
                  config.mockInitialSignal * ((occupiedCell.crowd ++ anotherCrowd).size + 2),
                  crowd,
                  occupiedCell.destinationPoint,
                  grid.workerId
                )
              case _ => occupiedCell
            }

          case _ =>
            throw new UnsupportedOperationException(s"Unresolved move, wtf bro?")
        }
      }

      val mock: MockCell = cell.asInstanceOf[MockCell]

      if (mock.crowd.nonEmpty) {
        val child =
          MockCell.create(
            initialSignal = config.mockInitialSignal * (mock.crowd.head.crowd.size + 1),
            initialCrowd = mock.crowd.head.crowd,
            destinationPoint = mock.crowd.head.destinationPoint,
            workerId = grid.workerId)
        makeMockMove(child)
        val occupiedCell =
          MockCell.create(
            initialSignal = config.mockInitialSignal * mock.crowd.size,
            mock.crowd.drop(1),
            mock.destinationPoint,
            workerId = grid.workerId
          )
        makeMockMove(occupiedCell)
      } else {
        val occupiedCell =
          MockCell.create(
            config.mockInitialSignal * (mock.crowd.size + 1),
            mock.crowd,
            mock.destinationPoint,
            workerId = grid.workerId
          )
        makeMockMove(occupiedCell)
      }
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

    val mockPopulation = dynamicCells.foldLeft(0)({ (acc, n) => acc + n._3.asInstanceOf[MockCell].crowd.size + 1 })
    val metrics = MockMetrics(mockPopulation, crowdOnProcessor, 0)
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