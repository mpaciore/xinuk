package pl.edu.agh.school.model.parallel

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.school.model._
import pl.edu.agh.school.simulation.SchoolMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object SchoolConflictResolver extends ConflictResolver[SchoolConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: SchoolConfig): (GridPart, SchoolMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), incomingCell) =>
        (incomingCell.withSmell(incomingCell.smell + currentSmell), SchoolMetrics.empty())
      case (currentCell: SmellingCell, EmptyCell(incomingSmell)) =>
        (currentCell.withSmell(currentCell.smell + incomingSmell), SchoolMetrics.empty())
      case (AlgaeCell(currentSmell, currentLifespan), ForaminiferaCell(energy, incomingSmell, incomingLifespan, pursuedSignalIndex)) =>
        (ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, incomingLifespan, pursuedSignalIndex), SchoolMetrics(0, 0, 0, 0, 0, 1, 0, currentLifespan))
      case (ForaminiferaCell(energy, currentSmell, currentLifespan, pursuedSignalIndex), AlgaeCell(incomingSmell, incomingLifespan)) =>
        (ForaminiferaCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, currentLifespan, pursuedSignalIndex), SchoolMetrics(0, 0, 0, 0, 0, 1, 0, incomingLifespan))
      case (AlgaeCell(currentSmell, lifespan), AlgaeCell(incomingSmell, incomingLifespan)) =>
        (AlgaeCell(currentSmell + incomingSmell, math.max(lifespan, incomingLifespan)), SchoolMetrics.empty())
      case (ForaminiferaCell(currentEnergy, currentSmell, lifespan, _), ForaminiferaCell(incomingEnergy, incomingSmell, incomingLifespan, pursuedSignalIndex)) => // TODO: here are place where two smells can intersect
        (ForaminiferaCell(currentEnergy + incomingEnergy, currentSmell + incomingSmell, math.max(lifespan, incomingLifespan), pursuedSignalIndex), SchoolMetrics.empty())
      case (Obstacle, _) => (Obstacle, SchoolMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
