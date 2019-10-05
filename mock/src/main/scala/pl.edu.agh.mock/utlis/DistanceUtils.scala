package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell, Point}
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid}
import scala.math.abs

object DistanceUtils  {
  def calculateDistance(firstPoint: LocalPoint, secondPoint: LocalPoint)(implicit config: MockConfig): Double = {
    val startingPoint = calculateGlobalPosition(firstPoint)
    val destinationPoint = calculateGlobalPosition(secondPoint)

    math.sqrt(math.pow(destinationPoint.x - startingPoint.x, 2) + math.pow(destinationPoint.y - startingPoint.y, 2))
  }

  def calculateGlobalPosition(point: LocalPoint)(implicit config: MockConfig): Point = {

    val globalX = (point.workerId.value - 1) / config.workersRoot
    val globalY = (point.workerId.value - 1) % config.workersRoot

    Point(globalX.asInstanceOf[Int] * config.gridSize + point.x, globalY.asInstanceOf[Int] * config.gridSize + point.y)
  }

  def calculateNeighboursDistances(cell: MockCell, x: Int, y: Int, grid: Grid, newGrid: Grid)(implicit config: MockConfig): Iterator[(Int, Int, Double)] = {
    val distanceCostsList = Grid.neighbourCellCoordinates(x, y)
      .map {
        case (i, j) => (i, j, calculateDistance(LocalPoint(i, j, cell.workerId), cell.destinationPoint))
      }

    val costList =
      distanceCostsList
        .map {
          case (_, _, cost) => cost
        }

    val (min, max) = costList.foldLeft((costList(0), costList(0))) {
      case ((min, max), e) => (if (min < e) min else e, if (max > e) max else e)
    }

    distanceCostsList
      .map {
        case (i, j, cost) =>
          (i, j, (cost - min) / (max - min) )
      }
      .iterator
      .filter(point =>{
        newGrid.cells(point._1)(point._2) match {
          case EmptyCell(_) => true
          case BufferCell(EmptyCell(_)) => true
          case BufferCell(MockCell(_,_,_,_)) => true
          case MockCell(_,_,_,_) => true
          case _ => false
        }
    })
  }

}
