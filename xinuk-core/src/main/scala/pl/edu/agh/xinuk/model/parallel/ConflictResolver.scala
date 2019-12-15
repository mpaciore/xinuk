package pl.edu.agh.xinuk.model.parallel

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.GridPart
import pl.edu.agh.xinuk.simulation.Metrics

trait ConflictResolver[T <: XinukConfig] {
  def resolveConflict(current: GridPart, incoming: GridPart)(implicit config: T): (GridPart, Metrics)
}