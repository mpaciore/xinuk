package pl.edu.agh.urban.model

import pl.edu.agh.urban.config.TargetType

case class Entrance(id: String, targetTypes: Set[TargetType], population: Long, lastDepartureTime: Double)
