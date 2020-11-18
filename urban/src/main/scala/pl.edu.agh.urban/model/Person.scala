package pl.edu.agh.urban.model

import java.util.UUID

import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.xinuk.model.{CellId, Direction}
import pl.edu.agh.xinuk.model.grid.GridDirection

case class Person(
                   source: String,
                   target: String,
                   travelMode: TravelMode,
                   wanderingSegmentEndTime: Double = 0d,
                   wanderingSegmentsRemaining: Long = 0L,
                   decisionHistory: Seq[(CellId, Direction)] = Seq.empty,
                   id: String = UUID.randomUUID().toString
                 ) {
  def createPersonMarker(round: Long)(implicit config: UrbanConfig): PersonMarker =
    PersonMarker(id, round)

  def withAddedDecision(cellId: CellId, direction: Direction)(implicit config: UrbanConfig): Person = {
    val shrunkHistory = decisionHistory.drop(decisionHistory.size + 1 - config.personMemorySize)
    copy(decisionHistory = shrunkHistory :+ (cellId, direction))
  }
}

object Person {
  def travelling(source: String, target: String)(implicit config: UrbanConfig): Person =
    Person(source, target, TravelMode.Travel)

  def wandering(source: String, target: String, time: Double)(implicit config: UrbanConfig): Person = {
    Person(source, target, TravelMode.Wander, time + config.randomSegmentDuration(), config.randomSegments() - 1)
  }
}

case class PersonMarker(personId: String, round: Long, distance: Double = 0d, sourceDirections: Set[Direction] = Set.empty) {
  def spread(spreadDirection: Direction)(implicit config: UrbanConfig): PersonMarker = {
    val opposite = spreadDirection.opposite
    val distanceAdded = config.scale * (if (spreadDirection.asInstanceOf[GridDirection].isDiagonal) math.sqrt(2d) else 1d)
    copy(distance = distance + distanceAdded, sourceDirections = Set(opposite))
  }
}

sealed trait TravelMode

object TravelMode {

  case object Travel extends TravelMode

  case object Return extends TravelMode

  case object Wander extends TravelMode

}
