package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Grid, Obstacle, Signal}

import scala.collection.immutable.TreeSet

object AlgorithmUtils {
  def mapDistances(grid: Grid)(implicit config: MockConfig): Unit = {
    grid.cells(0)(0) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), grid.workerId)
    var newGrid: Grid = Grid.empty(Set())

    for (
      x <- 0 until config.gridSize;
      y <- 0 until config.gridSize
    ) {
      newGrid.cells(x)(y) = grid.cells(x)(y)
    }

    newGrid.cells(2)(2) = Obstacle

    (0 until 200).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        newGrid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddends, x, y)
      )
      newGrid = Grid(cells, newGrid.workerId)
    }

    for (array <- newGrid.cells) {
      for (gridPart <- array) {
        for (smell <- gridPart.smell) {
          for(smellVal <- smell) {
            print(smellVal.value)
            print(", ")
          }
        }
        println()
      }
      println()
      println()
    }
  }

}
