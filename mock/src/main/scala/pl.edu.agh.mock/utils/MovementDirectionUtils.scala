package pl.edu.agh.mock.utils

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Point
import pl.edu.agh.xinuk.model.Signal

object MovementDirectionUtils {
  def calculateMovementCosts(
                              smellsList: Iterator[(Int, Int, Signal)],
                              distancesList: Iterator[(Int, Int, Double)]
                            )(implicit config: MockConfig): Iterator[(Int, Int, Double)] = {
    val list = smellsList
      .zip(distancesList)
      .map {
        case ((i, j, smell),(_, _, distance)) =>
          (i, j, smell, distance)
      }
      .map {
        case (i, j, smell, distance) =>
          (i, j, config.distanceFactor * distance + config.repulsionFactor * smell.value)
      }

    scala.util.Random.shuffle(list)
  }
  def calculateDirection(
                          movementCostList: Iterator[(Int, Int, Double)]
                        ): Point = {

    val destinationPoint = movementCostList
      .toStream
      .sortWith( _._3 < _._3)
      .head
    Point(destinationPoint._1,destinationPoint._2)
  }
}
