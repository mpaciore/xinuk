package pl.edu.agh.torch.model.parallel

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{EscapeCell, FireCell, HumanCell}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object TorchConflictResolver extends ConflictResolver[TorchConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: TorchConfig): GridPart = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) => incomingCell.withSmell(incomingCell.smell + currentSmell)
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) => currentCell.withSmell(currentCell.smell + incomingSmell)
      case (EscapeCell(currentSmell), HumanCell(_, _, _)) => EscapeCell(currentSmell)
      case (EscapeCell(_), FireCell(incomingCell)) => FireCell(incomingCell)
      case (FireCell(currentSmell), FireCell(incomingSmell)) => FireCell(currentSmell + incomingSmell)
      case (FireCell(currentSmell), HumanCell(incomingSmell, _, _)) => FireCell(currentSmell + incomingSmell)
      case (HumanCell(currentSmell, _, _), FireCell(incomingSmell)) => FireCell(currentSmell + incomingSmell)
      case (HumanCell(currentSmell, currentCrowd, currentSpeed), HumanCell(incomingSmell, incomingCrowd, _)) => HumanCell(currentSmell + incomingSmell, currentCrowd ++ incomingCrowd, currentSpeed)
      case (Obstacle, _) => Obstacle
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
