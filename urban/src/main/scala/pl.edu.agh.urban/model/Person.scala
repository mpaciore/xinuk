package pl.edu.agh.urban.model

import java.util.UUID

import pl.edu.agh.urban.config.UrbanConfig

case class Person(
                   source: String,
                   target: String,
                   travelMode: TravelMode,
                   wanderingSegmentTimeRemaining: Double = 0d,
                   wanderingSegmentsRemaining: Long = 0L,
                   id: String = UUID.randomUUID().toString
                 ) {
  def createPersonMarker(round: Long)(implicit config: UrbanConfig): PersonMarker =
    PersonMarker(id, round)
}

object Person {
  def travelling(source: String, target: String)(implicit config: UrbanConfig): Person =
    Person(source, target, TravelMode.Travel)

  def wandering(source: String, target: String)(implicit config: UrbanConfig): Person = {
    Person(source, target, TravelMode.Wander, config.randomSegmentDuration(), config.randomSegments() - 1)
  }
}

case class PersonMarker(personId: String, round: Long, distance: Int = 0) {
  def spread(): PersonMarker =
    copy(distance = distance + 1)
}

sealed trait TravelMode

object TravelMode {

  case object Travel extends TravelMode

  case object Wander extends TravelMode

}
