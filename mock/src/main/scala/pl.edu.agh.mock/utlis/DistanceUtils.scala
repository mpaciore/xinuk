package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell, Point}
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid}

object DistanceUtils  {
  def calculateDistance(firstPoint: LocalPoint, secondPoint: LocalPoint)(implicit config: MockConfig): Double = {
    val startingPoint = calculateGlobalPosition(firstPoint)
    val destinationPoint = calculateGlobalPosition(secondPoint)

    math.sqrt(math.pow(destinationPoint.x - startingPoint.x, 2) + math.pow(destinationPoint.y - startingPoint.y, 2))
  }

  def calculateGlobalPosition(point: LocalPoint)(implicit config: MockConfig): Point = {
    val globalY = scala.math.floor((point.workerId.value - 1) / config.workersRoot)
    val globalX = point.workerId.value - 1 - (config.workersRoot * globalY)

    Point(globalX.asInstanceOf[Int] * config.gridSize, globalY.asInstanceOf[Int] * config.gridSize)
  }

  def calculateNeighboursDistances(cell: MockCell, x: Int, y: Int, grid: Grid)(implicit config: MockConfig): Iterator[(Int, Int, Double)] = {
    val distanceCostsList = Grid.neighbourCellCoordinates(x, y)
      .map {
        case (i, j) => (i, j, calculateDistance(LocalPoint(x, y, cell.workerId), cell.destinationPoint))
      }

    val costList =
      distanceCostsList
        .map{
          case (_, _, cost) => cost
        }

    val min = costList.min
    val max = costList.max

    distanceCostsList
      .iterator
      .map {
        case (i, j, cost) =>
          (i, j, (cost - min)/(max - min))
      }
      .filter(point =>{
        grid.cells(point._1,point._2) match {
          case EmptyCell(_) => true
          case BufferCell(EmptyCell(_)) => true
          case _ => false
        }
    })
  }

}
