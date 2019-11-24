package pl.edu.agh.wind.algorithm

import pl.edu.agh.wind.config.WindConfig
import pl.edu.agh.wind.model.WindSourceCell
import pl.edu.agh.wind.simulation.WindMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model.{Grid, Obstacle}

import scala.collection.immutable.TreeSet

final class WindMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: WindConfig) extends MovesController {
  override def initialGrid: (Grid, WindMetrics) = {
    val grid = Grid.empty(bufferZone)
    val metrics = WindMetrics.empty()

    if (config.gridSize > 4) {
      for (i <- 1 to config.gridSize - 2) {
        grid.cells(1)(i) = Obstacle
        grid.cells(i)(config.gridSize - 2) = Obstacle

        if (i < config.gridSize - 4) {
         grid.cells(3)(i) = Obstacle
        }

        if (i > 2) {
          grid.cells(i)(config.gridSize - 4) = Obstacle
        }
      }
      grid.cells(2)(1) = Obstacle
      grid.cells(config.gridSize - 2)(config.gridSize - 3) = Obstacle

      grid.cells(2)(2) = WindSourceCell.create(config.windSourceInitialSignal)
    }

    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, WindMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    }
      newGrid.cells(x)(y) = grid.cells(x)(y)

    (newGrid, WindMetrics.empty())
  }
}
