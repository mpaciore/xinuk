package pl.edu.agh.formin.model.parallel

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.simulation.ForminMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object ForminConflictResolver extends ConflictResolver[ForminConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: ForminConfig): (GridPart, ForminMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), ForminMetrics.empty())
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), ForminMetrics.empty())
      case (AlgaeCell(currentSmell, currentLifespan), ForaminiferaCell(energy, incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, incomingLifespan), ForminMetrics(0, 0, 0, 0, 0, 1, 0, currentLifespan))
      case (ForaminiferaCell(energy, currentSmell, currentLifespan), AlgaeCell(incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, currentLifespan), ForminMetrics(0, 0, 0, 0, 0, 1, 0, incomingLifespan))
      case (AlgaeCell(currentSmell, lifespan), AlgaeCell(incomingSmell, incomingLifespan)) =>
        (AlgaeCell(currentSmell + incomingSmell, math.max(lifespan, incomingLifespan)), ForminMetrics.empty())
      case (ForaminiferaCell(currentEnergy, currentSmell, lifespan), ForaminiferaCell(incomingEnergy, incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(currentEnergy + incomingEnergy, currentSmell + incomingSmell, math.max(lifespan, incomingLifespan)), ForminMetrics.empty())
      case (Obstacle, _) => (Obstacle, ForminMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
