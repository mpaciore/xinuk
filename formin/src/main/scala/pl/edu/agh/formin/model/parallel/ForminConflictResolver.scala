package pl.edu.agh.formin.model.parallel

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver
import pl.edu.agh.xinuk.model._

object ForminConflictResolver extends ConflictResolver[ForminConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: ForminConfig): GridPart = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) => incomingCell.withSmell(incomingCell.smell + currentSmell)
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) => currentCell.withSmell(currentCell.smell + incomingSmell)
      case (AlgaeCell(currentSmell, _), ForaminiferaCell(energy, incomingSmell, lifespan)) => ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, lifespan)
      case (ForaminiferaCell(energy, incomingSmell, lifespan), AlgaeCell(currentSmell, _)) => ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, lifespan)
      case (AlgaeCell(currentSmell, lifespan), AlgaeCell(incomingSmell, incominglifespan)) => AlgaeCell(currentSmell + incomingSmell, math.max(lifespan, incominglifespan))
      case (ForaminiferaCell(currentEnergy, currentSmell, lifespan), ForaminiferaCell(incomingEnergy, incomingSmell, incominglifespan)) => ForaminiferaCell(currentEnergy + incomingEnergy, currentSmell + incomingSmell, math.max(lifespan, incominglifespan))
      case (Obstacle, _) => Obstacle
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
