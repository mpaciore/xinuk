package pl.edu.agh.formin.model.parallel

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._

trait ConflictResolver {
  def resolveConflict(current: Cell, incoming: SmellingCell)(implicit config: ForminConfig): Cell
}

object DefaultConflictResolver extends ConflictResolver {

  import Cell._

  override def resolveConflict(current: Cell, incoming: SmellingCell)(implicit config: ForminConfig): Cell = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) => incomingCell.withSmell(incomingCell.smell + currentSmell)
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) => currentCell.withSmell(currentCell.smell + incomingSmell)
      case (AlgaeCell(currentSmell), ForaminiferaCell(energy, incomingSmell)) => ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell)
      case (ForaminiferaCell(energy, incomingSmell), AlgaeCell(currentSmell)) => ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell)
      case (AlgaeCell(currentSmell), AlgaeCell(incomingSmell)) => AlgaeCell(currentSmell + incomingSmell)
      case (ForaminiferaCell(currentEnergy, currentSmell), ForaminiferaCell(incomingEnergy, incomingSmell)) => ForaminiferaCell(currentEnergy + incomingEnergy, currentSmell + incomingSmell)
      case (Obstacle, _) => Obstacle
    }
  }
}
