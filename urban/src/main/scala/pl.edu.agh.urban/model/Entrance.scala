package pl.edu.agh.urban.model

import pl.edu.agh.urban.config.TargetType

case class Entrance(targetId: String, targetTypes: Set[TargetType], population: Long, visitors: Seq[Visitor] = Seq.empty)

case class Visitor(person: Person, returnTime: Double)
