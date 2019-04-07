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
      case (StudentCell(currentSmell, currentLifespan, _), DirtCell(energy, incomingSmell, incomingLifespan, pursuedSignalIndex)) =>
        (DirtCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, incomingLifespan, pursuedSignalIndex), SchoolMetrics(0, 0, 0, 0, 0, 1, 0, currentLifespan))
      case (DirtCell(energy, currentSmell, currentLifespan, pursuedSignalIndex), StudentCell(incomingSmell, incomingLifespan, _)) =>
        (DirtCell(energy + config.algaeEnergeticCapacity, incomingSmell + currentSmell, currentLifespan, pursuedSignalIndex), SchoolMetrics(0, 0, 0, 0, 0, 1, 0, incomingLifespan))
      case (StudentCell(currentSmell, lifespan, signalIndex), StudentCell(incomingSmell, incomingLifespan, _)) =>
        (StudentCell(currentSmell + incomingSmell, math.max(lifespan, incomingLifespan), signalIndex), SchoolMetrics.empty())
      case (DirtCell(currentEnergy, currentSmell, lifespan, _), DirtCell(incomingEnergy, incomingSmell, incomingLifespan, pursuedSignalIndex)) => // TODO: here are place where two smells can intersect
        (DirtCell(currentEnergy + incomingEnergy, currentSmell + incomingSmell, math.max(lifespan, incomingLifespan), pursuedSignalIndex), SchoolMetrics.empty())
      case (Obstacle, _) => (Obstacle, SchoolMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
