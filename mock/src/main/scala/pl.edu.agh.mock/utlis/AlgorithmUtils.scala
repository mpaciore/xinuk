package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Grid, InitSmellPropagation, Obstacle, Signal}

object AlgorithmUtils {
  type DirectionalSmellArray = Array[Array[SmellArray]]
  var directionalSmellTopLeft: DirectionalSmellArray = _

  def mapDistances(grid: Grid)(implicit config: MockConfig): Unit = {

    var newGrid: Grid = Grid.empty(Set())

    for (
      x <- 0 until config.gridSize;
      y <- 0 until config.gridSize
    ) {
      newGrid.cells(x)(y) = grid.cells(x)(y)
    }

    newGrid.cells(0)(0) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), grid.workerId)

    //    newGrid.cells(0)(1) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), grid.workerId)
    //    newGrid.cells(0)(2) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), grid.workerId)
    //    newGrid.cells(0)(3) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), grid.workerId)

    newGrid.cells(2)(2) = Obstacle
    newGrid.cells(2)(1) = Obstacle

    (0 until config.gridSize*2).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        newGrid.propagatedSignal(InitSmellPropagation.calculateSmellAddends, x, y)
      )
      newGrid = Grid(cells, newGrid.workerId)
    }

    directionalSmellTopLeft = newGrid.cells
      .map(arrayOfGridParts => arrayOfGridParts.map(gridPart => gridPart.smell))
      .drop(1).dropRight(1).map(arrayOfGridParts => arrayOfGridParts.drop(1).dropRight(1))

    for (arrayOfArraysOfSmell <- directionalSmellTopLeft) {
      for (arrayOfSmell <- arrayOfArraysOfSmell) {
        for (smell <- arrayOfSmell) {
          smell.map(signal => signal.value).map(smellValue => print(smellValue + ", "))
          println()
        }
        println()
      }
      println()
    }

//    for (array <- newGrid.cells) {
//      for (gridPart <- array) {
//        for (smell <- gridPart.smell) {
//          for(smellVal <- smell) {
//            print(smellVal.value)
//            print(", ")
//          }
//        }
//        println()
//      }
//      println()
//      println()
//    }
  }

}