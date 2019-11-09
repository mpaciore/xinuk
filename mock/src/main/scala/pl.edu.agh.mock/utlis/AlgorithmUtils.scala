package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.mock.utlis.Direction.{Bottom, BottomLeft, BottomRight, Top, TopLeft, TopRight}
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Grid, InitSmellPropagation, Obstacle, Signal}

object Direction extends Enumeration {
  val TopLeft, Top, TopRight, Left, Right, BottomLeft, Bottom, BottomRight = Value

  def reversed(direction: Direction.Value): Direction.Value = {
    direction match {
      case TopLeft => BottomRight
      case Top => Bottom
      case TopRight => BottomLeft
      case Left => Right
      case Right => Left
      case BottomLeft => TopRight
      case Bottom => Top
      case BottomRight => TopLeft
    }
  }
}

object AlgorithmUtils {
  type DirectionalSmellArray = Array[Array[SmellArray]]

  var directionalSmell: Map[Direction.Value, DirectionalSmellArray] = Map[Direction.Value, DirectionalSmellArray]()

  def mapTransitionsThroughThisWorker(grid: Grid)(implicit config: MockConfig): Unit = {
    for (sourceDirection <- Direction.values) {
      val coordinatesToCheck = coordinatesToCheckFor(sourceDirection)
      for (destinationDirection <- Direction.values) {
        if (sourceDirection != destinationDirection) {
          val destinationDirectionalSmellArray: DirectionalSmellArray = directionalSmell(destinationDirection)
          var sumOfPositive = 0
          for (coordinateToCheck <- coordinatesToCheck) {
            val x: Int = coordinateToCheck._1
            val y: Int = coordinateToCheck._2
            val smellInCoordinateToCheck = destinationDirectionalSmellArray(x)(y)
            val numberOfPossibleDirections = smellInCoordinateToCheck.flatten.count(signal => signal.value != 0)
            if (numberOfPossibleDirections != 0) {
              sumOfPositive += 1
            }
          }
          if (sumOfPositive == coordinatesToCheck.size) {
            print("a")
          }
        }
      }
    }
  }

  def coordinatesToCheckFor(direction: Direction.Value)(implicit config: MockConfig): Array[(Int, Int)] = {
    val coordinates = direction match {
      case TopLeft => Array((1, 1))
      case Top => Array.range(1, config.gridSize - 1).map(num => (1, num))
      case TopRight => Array((1, config.gridSize - 2))
      case Direction.Left => Array.range(1, config.gridSize - 1).map(num => (num, 1))
      case Direction.Right => Array.range(1, config.gridSize - 1).map(num => (num, config.gridSize - 2))
      case BottomLeft => Array((config.gridSize - 2, 1))
      case Bottom => Array.range(1, config.gridSize - 1).map(num => (config.gridSize - 2, num))
      case BottomRight => Array((config.gridSize - 2, config.gridSize - 2))
    }
    coordinates.map(coordinate => (coordinate._1 - 1, coordinate._2 - 1))
  }

  def mapLocalDistancesForEveryDirection(grid: Grid)(implicit config: MockConfig): Unit = {
    Direction.values.foreach(direction => {
      mapDistances(grid, direction)
    })
  }

  def mapDistances(grid: Grid, direction: Direction.Value)(implicit config: MockConfig): Unit = {
    var newGrid: Grid = Grid.empty(Set())

    for (
      x <- 0 until config.gridSize;
      y <- 0 until config.gridSize
    ) {
      newGrid.cells(x)(y) = grid.cells(x)(y)
    }

    val coordinates = initialMockCoordinatesFor(direction)

    for (coordinate <- coordinates) {
      newGrid.cells(coordinate._1)(coordinate._2) = MockCell.create(Signal(1), List(), LocalPoint(1, 1, grid.workerId), grid.workerId)
    }

    newGrid.cells(2)(2) = Obstacle

    (0 until config.gridSize*2).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        newGrid.propagatedSignal(InitSmellPropagation.calculateSmellAddends, x, y)
      )
      newGrid = Grid(cells, newGrid.workerId)
    }

    directionalSmell += (
      direction -> newGrid.cells
        .map(arrayOfGridParts => arrayOfGridParts.map(gridPart => gridPart.smell))
        .drop(1).dropRight(1).map(arrayOfGridParts => arrayOfGridParts.drop(1).dropRight(1))
    )

//  Printing directional signal for debugging purposes
//
//    println()
//    println(direction.toString())
//    println()
//    for (arrayOfArraysOfSmell <- directionalSmell(direction)) {
//      for (arrayOfSmell <- arrayOfArraysOfSmell) {
//        for (smell <- arrayOfSmell) {
//          smell.map(signal => signal.value).map(smellValue => print(smellValue + ", "))
//          println()
//        }
//        println()
//      }
//      println()
//    }
  }

  def initialMockCoordinatesFor(direction: Direction.Value)(implicit config: MockConfig): Array[(Int, Int)] = {
    direction match {
      case TopLeft => Array((0, 0))
      case Top => Array.range(1, config.gridSize - 1).map(num => (0, num))
      case TopRight => Array((0, config.gridSize - 1))
      case Direction.Left => Array.range(1, config.gridSize - 1).map(num => (num, 0))
      case Direction.Right => Array.range(1, config.gridSize - 1).map(num => (num, config.gridSize - 1))
      case BottomLeft => Array((config.gridSize - 1, 0))
      case Bottom => Array.range(1, config.gridSize - 1).map(num => (config.gridSize - 1, num))
      case BottomRight => Array((config.gridSize - 1, config.gridSize - 1))
    }
  }
}