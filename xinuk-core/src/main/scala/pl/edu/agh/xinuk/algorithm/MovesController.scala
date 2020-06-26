package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.EnhancedGrid
import pl.edu.agh.xinuk.simulation.Metrics

trait MovesController[T <: XinukConfig] {
  def makeMoves(iteration: Long, grid: EnhancedGrid)(implicit config: T): (EnhancedGrid, Metrics)
}
