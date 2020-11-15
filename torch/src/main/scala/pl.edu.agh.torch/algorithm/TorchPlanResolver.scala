package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.algorithm.TorchUpdate.{AddPerson, CreateFire, RemovePerson}
import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{Exit, Fire, Person}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, Update}
import pl.edu.agh.xinuk.model.{CellContents, Empty}

final case class TorchPlanResolver() extends PlanResolver[TorchConfig] {
  override def isUpdateValid(iteration: Long, contents: CellContents, update: Update)(implicit config: TorchConfig): Boolean =
    (contents, update) match {
      case (_: Person, RemovePerson) => true

      case (Exit, CreateFire) => true
      case (Empty, CreateFire) => true
      case (_: Person, CreateFire) => true

      case (Fire, _: AddPerson) => true
      case (Empty, _: AddPerson) => true
      case (Exit, _: AddPerson) => true

      case _ => false
    }

  override def applyUpdate(iteration: Long, contents: CellContents, update: Update)(implicit config: TorchConfig): (CellContents, TorchMetrics) = {
    val (newContents: CellContents, metrics: TorchMetrics) = (contents, update) match {
      case (_: Person, RemovePerson) =>
        (Empty, TorchMetrics.empty)

      case (Exit, CreateFire) =>
        (Fire, TorchMetrics.fire)
      case (Empty, CreateFire) =>
        (Fire, TorchMetrics.fire)
      case (_: Person, CreateFire) =>
        (Fire, TorchMetrics.fire + TorchMetrics.death)

      case (Fire, _: AddPerson) =>
        (Fire, TorchMetrics.death)
      case (Exit, _: AddPerson) =>
        (Exit, TorchMetrics.escape)
      case (Empty, AddPerson(person)) =>
        (person, TorchMetrics.empty)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: state = $contents, update = $update")
    }

    (newContents, metrics)
  }
}

