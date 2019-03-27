package pl.edu.agh.mock.model.parallel

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object MockConflictResolver extends ConflictResolver[MockConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: MockConfig): (GridPart, MockMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) =>
        (Obstacle, MockMetrics.empty())
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), MockMetrics.empty())
      case (MockCell(currentSmell), EmptyCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell), MockMetrics.empty())
      case (EmptyCell(currentSmell), MockCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell), MockMetrics.empty())
      case (MockCell(currentSmell), MockCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell), MockMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
