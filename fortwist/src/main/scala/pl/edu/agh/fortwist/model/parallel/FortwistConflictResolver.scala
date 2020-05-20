package pl.edu.agh.fortwist.model.parallel

import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.FortwistCell
import pl.edu.agh.fortwist.simulation.FortwistMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object FortwistConflictResolver extends ConflictResolver[FortwistConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: GridPart)(implicit config: FortwistConfig): (GridPart, FortwistMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) => (Obstacle, FortwistMetrics.empty())
      case (FortwistCell(currentSmell, currentForaminiferas, currentAlgae), FortwistCell(incomingSmell, incomingForaminiferas, incomingAlgae)) =>
        (FortwistCell(currentSmell + incomingSmell, currentForaminiferas ++ incomingForaminiferas, currentAlgae + incomingAlgae), FortwistMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
