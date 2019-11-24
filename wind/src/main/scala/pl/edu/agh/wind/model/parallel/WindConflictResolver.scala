package pl.edu.agh.wind.model.parallel

import pl.edu.agh.wind.config.WindConfig
import pl.edu.agh.wind.model.WindSourceCell
import pl.edu.agh.wind.simulation.WindMetrics
import pl.edu.agh.xinuk.model.parallel.ConflictResolver
import pl.edu.agh.xinuk.model._

object WindConflictResolver extends ConflictResolver[WindConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: WindConfig): (GridPart, WindMetrics) = {
    (current, incoming) match {

      case (Obstacle, _) =>
        (Obstacle, WindMetrics.empty())

      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), WindMetrics.empty())

      case (WindSourceCell(currentSmell), EmptyCell(incomingSmell)) =>
        (WindSourceCell(currentSmell + incomingSmell), WindMetrics.empty())

      case (EmptyCell(currentSmell), WindSourceCell(incomingSmell)) =>
        (WindSourceCell(currentSmell + incomingSmell), WindMetrics.empty())

      case (WindSourceCell(currentSmell), WindSourceCell(incomingSmell)) =>
        (WindSourceCell(currentSmell + incomingSmell), WindMetrics.empty())

      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
