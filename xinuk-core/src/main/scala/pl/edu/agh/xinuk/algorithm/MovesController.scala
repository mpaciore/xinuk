package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.EnhancedGrid
import pl.edu.agh.xinuk.simulation.Metrics

trait MovesController {
  def makeMoves(iteration: Long, grid: EnhancedGrid): (EnhancedGrid, Metrics)
}
