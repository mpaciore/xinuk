package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.model.{Person, PersonMarker}
import pl.edu.agh.xinuk.algorithm.Update

object UrbanUpdate {

  case class AddMarker(marker: PersonMarker) extends Update

  case class PurgeMarkers(round: Long) extends Update

  case class CreatePerson(person: Person, round: Long) extends Update

  case class AddPerson(person: Person, round: Long) extends Update

  case class KeepPerson(person: Person, round: Long) extends Update

  case class RemovePerson(personId: String) extends Update

  case class RemoveVisitor(targetId: String, visitorId: String) extends Update
}
