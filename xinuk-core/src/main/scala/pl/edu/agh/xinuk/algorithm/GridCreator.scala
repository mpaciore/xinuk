package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{Grid, NonPlanarConnections}

trait GridCreator[T <: XinukConfig] {
  def initialGrid(implicit config: T): (Grid, NonPlanarConnections)
}
