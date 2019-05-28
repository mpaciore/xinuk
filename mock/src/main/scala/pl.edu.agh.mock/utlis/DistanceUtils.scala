package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, Point}

object DistanceUtils {
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

}
