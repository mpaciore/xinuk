package pl.edu.agh.xinuk.model.parallel

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{GridPart, SmellingCell}
import pl.edu.agh.xinuk.simulation.Metrics

trait ConflictResolver[T <: XinukConfig] {
  def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: T): (GridPart, Metrics)
}