package pl.edu.agh.fortwist.model.parallel

import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object FortwistConflictResolver extends ConflictResolver[FortwistConfig] {

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: FortwistConfig): GridPart = {
    (current, incoming) match {
      case (Obstacle, _) => Obstacle
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
