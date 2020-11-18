package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.algorithm.UrbanUpdate._
import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.urban.model._
import pl.edu.agh.xinuk.algorithm.{PlanResolver, Update}
import pl.edu.agh.xinuk.model.CellContents

final case class UrbanPlanResolver() extends PlanResolver[UrbanConfig] {


  override def isUpdateValid(iteration: Long, contents: CellContents, update: Update)
                            (implicit config: UrbanConfig): Boolean = {
    (contents, update) match {
      case (UrbanCell(_, _, _, markers), AddMarker(newMarker)) =>
        canAddMarker(markers, newMarker)
      case (_: UrbanCell, _: PurgeMarkers) =>
        true

      case (_: UrbanCell, _: CreatePerson) =>
        true
      case (_: UrbanCell, _: AddPerson) =>
        true
      case (UrbanCell(_, _, occupants, _), KeepPerson(personToKeep, _)) =>
        occupants.exists(_.id == personToKeep.id)
      case (UrbanCell(_, _, occupants, _), RemovePerson(personId)) =>
        occupants.exists(_.id == personId)

      case (UrbanCell(_, entrances, _, _), RemoveVisitor(targetId, personId)) =>
        entrances.exists { entrance =>
          entrance.targetId == targetId && entrance.visitors.exists(_.person.id == personId)
        }

      case _ =>
        false
    }
  }

  private def canAddMarker(markers: Seq[PersonMarker], newMarker: PersonMarker): Boolean = !markers.exists {
    marker => marker.personId == newMarker.personId && marker.round == newMarker.round && marker.distance <= newMarker.distance
  }

  override def applyUpdate(iteration: Long, contents: CellContents, update: Update)
                          (implicit config: UrbanConfig): (CellContents, UrbanMetrics) = {
    (contents, update) match {
      case (cell: UrbanCell, AddMarker(newMarker)) =>
        val newMarkers = cell.markers.filterNot {
          marker => marker.personId == newMarker.personId && marker.round == newMarker.round && marker.distance > newMarker.distance
        } :+ newMarker
        (cell.copy(markers = newMarkers), UrbanMetrics.empty)
      case (cell: UrbanCell, PurgeMarkers(round)) =>
        // remove markers older than this and previous round
        val newMarkers = cell.markers.filterNot(_.round < round - 1)
        (cell.copy(markers = newMarkers), UrbanMetrics.empty)

      case (cell: UrbanCell, CreatePerson(person, round)) =>
        val (newContents, metrics) = handleIncomingPerson(iteration, cell, person, round)
        val creationMetrics = person.travelMode match {
          case TravelMode.Travel =>
            UrbanMetrics.travelBeginning
          case TravelMode.Return =>
            UrbanMetrics.returnBeginning
          case TravelMode.Wander =>
            UrbanMetrics.wanderBeginning
        }
        (newContents, metrics + creationMetrics)
      case (cell: UrbanCell, AddPerson(person, round)) =>
        handleIncomingPerson(iteration, cell, person, round)
      case (cell: UrbanCell, KeepPerson(personToKeep, round)) =>
        val newOccupants = cell.occupants.filterNot(_.id == personToKeep.id) :+ personToKeep
        val newMarkers = cell.markers :+ personToKeep.createPersonMarker(round)
        (cell.copy(occupants = newOccupants, markers = newMarkers), UrbanMetrics.empty)
      case (cell: UrbanCell, RemovePerson(personId)) =>
        val newOccupants = cell.occupants.filterNot(_.id == personId)
        (cell.copy(occupants = newOccupants), UrbanMetrics.empty)

      case (cell@UrbanCell(_, entrances, _, _), RemoveVisitor(targetId, personId)) =>
        val modifiedEntrance = entrances.find(_.targetId == targetId).get
        val newVisitors = modifiedEntrance.visitors.filterNot(_.person.id == personId)
        val newEntrances = entrances.filterNot(_.targetId == targetId) :+ modifiedEntrance.copy(visitors = newVisitors)
        (cell.copy(entrances = newEntrances), UrbanMetrics.empty)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }
  }

  private def handleIncomingPerson(iteration: Long, cell: UrbanCell, person: Person, markerRound: Long)
                                  (implicit config: UrbanConfig): (CellContents, UrbanMetrics) = {
    cell.entrances.find(_.targetId == person.target).map { entrance =>
      person.travelMode match {
        case TravelMode.Travel =>
          // person arrived at destination and is visiting
          val timeState = config.getTimeState(iteration)
          (addVisitor(timeState.time, cell, entrance, person), UrbanMetrics.travelEnd)
        case TravelMode.Return =>
          // person returned to source
          (cell, UrbanMetrics.returnEnd)
        case TravelMode.Wander =>
          // person is wandering until wander target is changed
          (putPerson(cell, person, markerRound), UrbanMetrics.empty)
      }
    }.getOrElse((putPerson(cell, person, markerRound), UrbanMetrics.empty)) // no entrance in cell is the target of person
  }

  private def addVisitor(time: Double, cell: UrbanCell, entrance: Entrance, person: Person)
                        (implicit config: UrbanConfig): UrbanCell = {
    val newVisitor = Visitor(person, time + visitDuration(entrance))
    val newEntrances = cell.entrances.filterNot(_.targetId == entrance.targetId) :+ entrance.copy(visitors = entrance.visitors :+ newVisitor)
    cell.copy(entrances = newEntrances)
  }

  private def putPerson(cell: UrbanCell, person: Person, markerRound: Long)
                       (implicit config: UrbanConfig): CellContents = {
    val newOccupants = cell.occupants :+ person
    val newMarkers = cell.markers :+ person.createPersonMarker(markerRound)
    cell.copy(occupants = newOccupants, markers = newMarkers)
  }

  private def visitDuration(entrance: Entrance)(implicit config: UrbanConfig): Double = {
    config.randomVisitTime(entrance.targetTypes.toSeq(config.random.nextInt(entrance.targetTypes.size)))
  }
}