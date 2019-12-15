package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{Grid, NonPlanarConnections}

trait GridCreator {
  def initialGrid: (Grid, NonPlanarConnections)
}
