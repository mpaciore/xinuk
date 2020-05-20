package pl.edu.agh.torch.model.parallel

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{EscapeCell, FireCell, HumanCell}
import pl.edu.agh.torch.simulation.TorchMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object TorchConflictResolver extends ConflictResolver[TorchConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: GridPart)(implicit config: TorchConfig): (GridPart, TorchMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), TorchMetrics.empty())
      case (currentCell: GridPart, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), TorchMetrics.empty())
      case (EscapeCell(currentSmell), HumanCell(_, _, _)) =>
        (EscapeCell(currentSmell), TorchMetrics(0, 0, 0, 0, 1))
      case (EscapeCell(_), FireCell(incomingCell)) =>
        (FireCell(incomingCell), TorchMetrics.empty())
      case (FireCell(currentSmell), FireCell(incomingSmell)) =>
        (FireCell(currentSmell + incomingSmell), TorchMetrics.empty())
      case (FireCell(currentSmell), HumanCell(incomingSmell, _, _)) =>
        (FireCell(currentSmell + incomingSmell), TorchMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, _, _), FireCell(incomingSmell)) =>
        (FireCell(currentSmell + incomingSmell), TorchMetrics(0, 0, 0, 1, 0))
      case (HumanCell(currentSmell, currentCrowd, currentSpeed), another@HumanCell(incomingSmell, incomingCrowd, _)) =>
        (HumanCell(currentSmell + incomingSmell, currentCrowd ++ incomingCrowd ++ List(another), currentSpeed), TorchMetrics.empty())
      case (Obstacle, _) => (Obstacle, TorchMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
