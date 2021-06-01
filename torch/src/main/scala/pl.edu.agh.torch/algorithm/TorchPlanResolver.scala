package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{Exit, Fire, Person}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, Empty, Obstacle}

final case class TorchPlanResolver() extends PlanResolver[TorchConfig] {
  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: TorchConfig): Boolean =
    (contents, update.value) match {
      case (Obstacle, _) => false           // cannot update Obstacle
      case (_, Obstacle) => false           // cannot update with Obstacle
      case (_, Exit) => false               // cannot update with Exit
      case (_, Empty) => true               // update with Empty has special meaning
      case (Fire, _) => false               // cannot update Fire except Empty update
      case (_, Fire) => true                // Fire can update anything except Obstacle and Fire
      case (Person(_), Person(_)) => false  // People cannot crowd
      case _ => true                        // anything else should be possible
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: TorchConfig): (CellContents, TorchMetrics) = {

    val (newContents: CellContents, metrics: TorchMetrics) = (contents, update.value) match {
      case (_, Empty) =>
        // Empty update should be treated as the old contents leaving the cell
        (Empty, TorchMetrics.empty)

      case (Person(_), Fire) =>
        (Fire, TorchMetrics.death + TorchMetrics.fire)
      case (_, Fire) =>
        (Fire, TorchMetrics.fire)

      case (Fire, Person(_)) =>
        (Fire, TorchMetrics.death)
      case (Exit, Person(_)) =>
        (Exit, TorchMetrics.escape)
      case (Empty, person: Person) =>
        (person, TorchMetrics.empty)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: state = $contents, update = $update")
    }

    (newContents, metrics)
  }
}

