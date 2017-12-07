package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.Grid

trait MovesController {
  def initialGrid: Grid

  def makeMoves(iteration: Long, grid: Grid): Grid
}
