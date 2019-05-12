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
      case (MockCell(currentSmell, currentCrowd, destinationPoint), EmptyCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell, currentCrowd, destinationPoint), MockMetrics.empty())
      case (EmptyCell(currentSmell), MockCell(incomingSmell, incomingCrowd, destinationPoint)) =>
        (MockCell(currentSmell + incomingSmell, incomingCrowd, destinationPoint), MockMetrics.empty())
      case (MockCell(currentSmell, currentCrowd, destinationPoint), MockCell(incomingSmell, incomingCrowd, _)) =>
        (MockCell(currentSmell + incomingSmell, currentCrowd + incomingCrowd, destinationPoint), MockMetrics(currentCrowd + incomingCrowd))
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}