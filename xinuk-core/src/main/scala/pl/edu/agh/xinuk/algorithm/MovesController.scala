package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.Grid
import pl.edu.agh.xinuk.simulation.Metrics

trait MovesController {
  def initialGrid: (Grid, Metrics)

  def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics)
}
