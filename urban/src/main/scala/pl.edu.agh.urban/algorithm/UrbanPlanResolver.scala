package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.algorithm.UrbanUpdate._
import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.urban.model.{Person, PersonMarker, TravelMode, UrbanCell}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, Update}
import pl.edu.agh.xinuk.model.CellContents

final case class UrbanPlanResolver() extends PlanResolver[UrbanConfig] {


  override def isUpdateValid(contents: CellContents, update: Update)(implicit config: UrbanConfig): Boolean = {
    (contents, update) match {
      case (UrbanCell(_, _, _, markers), AddMarker(newMarker)) => canAddMarker(markers, newMarker)
      case (_: UrbanCell, _: PurgeMarkers) => true

      case (UrbanCell(_, _, None, _), _: CreatePerson) => true
      case (UrbanCell(_, _, None, _), _: AddPerson) => true
      case (UrbanCell(_, _, Some(person: Person), _), KeepPerson(personToKeep, _)) => person.id == personToKeep.id
      case (UrbanCell(_, _, Some(person: Person), _), RemovePerson(personId)) => person.id == personId

//      case (UrbanCell(_, Some(_), _, _), _: UpdateEntrance) => true

      case _ => false
    }
  }

  private def canAddMarker(markers: Seq[PersonMarker], newMarker: PersonMarker): Boolean = !markers.exists {
    marker => marker.personId == newMarker.personId && marker.round == newMarker.round && marker.distance <= newMarker.distance
  }

  override def applyUpdate(contents: CellContents, update: Update)(implicit config: UrbanConfig): (CellContents, UrbanMetrics) = {
    (contents, update) match {
      case (cell: UrbanCell, AddMarker(newMarker)) =>
        val newMarkers = cell.markers.filterNot {
          marker => marker.personId == newMarker.personId && marker.round == newMarker.round && marker.distance > newMarker.distance
        } :+ newMarker
        (cell.copy(markers = newMarkers), UrbanMetrics.empty)
      case (cell: UrbanCell, PurgeMarkers(round)) =>
        val newMarkers = cell.markers.filterNot(_.round < round)
        (cell.copy(markers = newMarkers), UrbanMetrics.empty)

      case (cell: UrbanCell, CreatePerson(person, round)) =>
        val (newContents, metrics) = addPerson(cell, person, round)
        (newContents, metrics + UrbanMetrics.personCreated)
      case (cell: UrbanCell, AddPerson(person, round)) =>
        addPerson(cell, person, round)
      case (cell: UrbanCell, KeepPerson(person, round)) =>
        (cell.copy(occupant = Some(person), markers = cell.markers :+ person.createPersonMarker(round)), UrbanMetrics.empty)
      case (cell: UrbanCell, RemovePerson(_)) =>
        (cell.copy(occupant = None), UrbanMetrics.empty)

//      case (cell@UrbanCell(_, Some(entrance), _, _), UpdateEntrance(lastDepartureTime)) =>
//        (cell.copy(entrance = Some(entrance.copy(lastDepartureTime = lastDepartureTime))), UrbanMetrics.empty)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }
  }

  private def addPerson(cell: UrbanCell, person: Person, markerRound: Long)
                       (implicit config: UrbanConfig): (CellContents, UrbanMetrics) = {
    if (person.travelMode == TravelMode.Travel && cell.entrance.isDefined && cell.entrance.get.id == person.target) {
      (cell, UrbanMetrics.personRemoved)
    } else {
      (cell.copy(occupant = Some(person), markers = cell.markers :+ person.createPersonMarker(markerRound)), UrbanMetrics.empty)
    }
  }
}