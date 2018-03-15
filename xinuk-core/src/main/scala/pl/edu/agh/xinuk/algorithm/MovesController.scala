package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{Grid, GridPart, Signal}
import pl.edu.agh.xinuk.simulation.Metrics

trait MovesController {
  def initialGrid: (Grid, Metrics)

  def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics)

  protected final def calculatePossibleDestinations(x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => grid.cells(x)(y).smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }
}
