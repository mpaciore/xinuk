package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model._
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.mock.utlis.{AStartAlgorithmUtils, AlgorithmUtils, Direction, DistanceUtils, GridUtils, MovementDirectionUtils, SmellUtils}
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Obstacle, _}
import scala.collection.immutable.TreeSet

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  var crowdOnProcessor = 0
  var transitionsThroughWorkers: Map[Int, Map[(Direction.Value, Direction.Value), Boolean]] = Map[Int, Map[(Direction.Value, Direction.Value), Boolean]]()
  val algorithmUtils = new AlgorithmUtils()
  var receivedMessages = 0

  override def receiveMessage(message: Any): Unit = {
    val tuple = message.asInstanceOf[(Int, Map[(Direction.Value, Direction.Value), Boolean])]
    transitionsThroughWorkers += (tuple._1 -> tuple._2)
    receivedMessages += 1
    if (receivedMessages == math.pow(config.workersRoot, 2).toInt) {
      println(transitionsThroughWorkers)
    }
  }

  override def initialGrid(workerId: WorkerId): (Grid, MockMetrics, Map[(Direction.Value, Direction.Value), Boolean]) = {
    val grid = Grid.empty(bufferZone,workerId = workerId)

    GridUtils.loadDataFromFile("map.json", grid)
    //grid.cells(1)(0) = Obstacle

    algorithmUtils.mapLocalDistancesForEveryDirection(grid)
    algorithmUtils.mapTransitionsThroughThisWorker(grid)

//    grid.cells(5)(5) = Obstacle
//    grid.cells(4)(5) = Obstacle
//    grid.cells(5)(4) = Obstacle
//    grid.cells(5)(3) = Obstacle
//    grid.cells(5)(2) = Obstacle
//
//    AStartAlgorithmUtils.aStar((1,1), (13,12), grid)
//        .foreach(println)

    Thread.sleep(1000)

    if(grid.cells(config.gridSize / 2)(config.gridSize / 2).isInstanceOf[EmptyCell]) {
      grid.cells(config.gridSize / 2)(config.gridSize / 2) =
        MockCell.create(config.mockInitialSignal,
          destinationPoint = POIFactory.generatePOI(grid),
          workerId = grid.workerId)
    }

    val metrics = MockMetrics.empty()
    (grid, metrics, algorithmUtils.transitionsThroughThisWorker)
  }


  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {

    val newGrid = Grid.empty(bufferZone, workerId = grid.workerId)
    Thread.sleep(10)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {

      def makeMockMove(occupiedCell: MockCell): Unit = {
        if (occupiedCell.destinationPoint == LocalPoint(x,y,occupiedCell.workerId) || !isDestinationPointAccessible(grid,occupiedCell) ) {
          occupiedCell.destinationPoint = POIFactory.generatePOI(grid)
        }
        val point =
          MovementDirectionUtils.calculateDirection(
            MovementDirectionUtils.calculateMovementCosts(
              SmellUtils.calculateNeighboursSmell(occupiedCell, x , y, grid, newGrid),
              DistanceUtils.calculateNeighboursDistances(occupiedCell, x , y, grid, newGrid)
            )
          )
        val destination = Tuple2(point.x, point.y)
        val vacatedCell = EmptyCell(cell.smell)
        newGrid.cells(destination._1)(destination._2) match {
          case EmptyCell(_) =>
            newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
              case occupied@MockCell(_, _, _, _) => occupied
              case _ => vacatedCell
            }
            newGrid.cells(destination._1)(destination._2) =
              MockCell(
                occupiedCell.smell ++ grid.cells(destination._1)(destination._1).smell,
                occupiedCell.crowd,
                occupiedCell.destinationPoint,
                occupiedCell.workerId
              )

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

      val newSmell = mock.smell.map {
        arr: Array[Signal] =>
          arr.map {
            sig: Signal => Signal(sig.value + config.mockInitialSignal.value)
          }
      }

      // TODO: splitting whole crowd
      if (mock.crowd.nonEmpty) {
        val singlePedestrianFromCrowd =
          MockCell.create(
            initialSignal = config.mockInitialSignal,
            initialCrowd = mock.crowd.head.crowd,
            destinationPoint = mock.crowd.head.destinationPoint,
            workerId = grid.workerId
          ).withSmell(newSmell)
        makeMockMove(singlePedestrianFromCrowd)

        val mockWithoutOneCrowdPerson =
          MockCell.create(
            initialSignal = config.mockInitialSignal * mock.crowd.size,
            mock.crowd.drop(1),
            mock.destinationPoint,
            workerId = grid.workerId
          ).withSmell(newSmell)

        newGrid.cells(x)(y) = newGrid.cells(x)(y)  match {
          case newPedestrian@MockCell(_,_,_,_) =>
            MockCell.create(
              initialSignal = config.mockInitialSignal * mock.crowd.size,
              mockWithoutOneCrowdPerson.crowd ++ List(newPedestrian),
              mock.destinationPoint,
              workerId = grid.workerId
            ).withSmell(newSmell)
           case _ =>
              mockWithoutOneCrowdPerson
        }


      } else {
        val occupiedCell =
          MockCell.create(
            config.mockInitialSignal * (mock.crowd.size + 1),
            mock.crowd,
            mock.destinationPoint,
            workerId = grid.workerId
          ).withSmell(newSmell)
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
    if (point.workerId.value != cell.workerId.value) return true
    grid.cells(point.x)(point.y) match {
      case Obstacle => false
      case _ => true
    }
  }


}