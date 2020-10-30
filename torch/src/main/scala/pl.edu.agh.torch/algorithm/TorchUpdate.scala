package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.model.Person
import pl.edu.agh.xinuk.algorithm.Update

object TorchUpdate {
  case object CreateFire extends Update

  case class AddPerson(person: Person) extends Update

  case object RemovePerson extends Update
}
