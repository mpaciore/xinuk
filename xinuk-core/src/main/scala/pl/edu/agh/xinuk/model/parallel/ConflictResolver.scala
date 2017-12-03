package pl.edu.agh.xinuk.model.parallel

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{Cell, SmellingCell}

trait ConflictResolver[T <: XinukConfig] {
  def resolveConflict(current: Cell, incoming: SmellingCell)(implicit config: T): Cell
}