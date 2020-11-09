package pl.edu.agh.urban.model

import java.util.UUID

import pl.edu.agh.urban.config.UrbanConfig

case class Person(
                   source: String,
                   target: Option[String] = Option.empty,
                   mode: TravelMode = TravelMode.Standard,
                   id: String = UUID.randomUUID().toString
                 ) {
  def createPersonMarker(round: Long)(implicit config: UrbanConfig): PersonMarker = PersonMarker(id, round)
}

case class PersonMarker(personId: String, round: Long, distance: Int = 0) {
  def spread(): PersonMarker = copy(distance = distance + 1)
}

sealed trait TravelMode

object TravelMode {

  case object Standard extends TravelMode

  case object Wander extends TravelMode

}
