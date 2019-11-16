package pl.edu.agh.mock.utils

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.model.{SimulationMap, Tile}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, GridPart, Obstacle, WorkerId}

object GridUtils extends LazyLogging{

  def loadDataFromFile(filename: String, grid: Grid)(implicit config: XinukConfig): Unit = {
    val simulationMap: SimulationMap = JsonMapParser.parseMapFromJson(filename)
    val gridArray = simulationMap.getTilesAsArray
    val xOffset = calculateXOffset(grid.workerId, config.workersRoot, config.gridSize)
    val yOffset = calculateYOffset(grid.workerId, config.workersRoot, config.gridSize)
    for (i <- 0 until config.gridSize; j <- 0 until config.gridSize) {
      grid.cells(i)(j) match {
        case EmptyCell.Instance => grid.cells(i)(j) = gridArray(i + xOffset)(j + yOffset)
        case BufferCell(EmptyCell(_)) =>
          if (gridArray(i + xOffset)(j + yOffset).isInstanceOf[Obstacle.type]) {
            grid.cells(i)(j) = Obstacle
          }
        case _ =>
      }
    }

    updateBufferZone(grid, gridArray, xOffset, yOffset)
  }
  //TODO: Refactor this method
  private def updateBufferZone(grid: Grid, gridArray: Array[Array[GridPart]], xOffset: Int, yOffset: Int)
                              (implicit config: XinukConfig) = {
    for (i <- 0 until config.gridSize) {
      //Left side
      grid.cells(i)(1) match {
        case Obstacle => grid.cells(i)(0) = Obstacle
        case _ =>
      }

      //Right side
      grid.cells(i)(config.gridSize - 2) match {
        case Obstacle => grid.cells(i)(config.gridSize - 1) = Obstacle
        case _ =>
      }

      //Top
      grid.cells(1)(i) match {
        case Obstacle => grid.cells(0)(i) = Obstacle
        case _ =>
      }

      //Bottom
      grid.cells(config.gridSize - 2)(i) match {
        case Obstacle => grid.cells(config.gridSize - 1)(i) = Obstacle
        case _ =>
      }
    }

    // Update left buffer zone
    if (yOffset > 0) {
      for (i <- 0 until config.gridSize; j <- 2 to 1 by -1) {
        gridArray(xOffset + i)(yOffset - j) match {
          case Obstacle => grid.cells(i)(0) = Obstacle
          case _ =>
        }
      }
    }

    // Update top buffer zone
    if (xOffset > 0) {
      for (i <- 0 until config.gridSize; j <- 2 to 1 by -1) {
        gridArray(xOffset - j)(yOffset + i) match {
          case Obstacle => grid.cells(0)(i) = Obstacle
          case _ =>
        }
      }
    }

    //Update right buffer zone
    if (grid.workerId.value % config.workersRoot != 0) {
      for (i <- 0 until config.gridSize; j <- 0 to 1) {
        gridArray(xOffset + i)(yOffset + config.gridSize + j) match {
          case Obstacle => grid.cells(i)(config.gridSize - 1) = Obstacle
          case _ =>
        }
      }
    }

    //Update bottom buffer zone
    if (grid.workerId.value <= (Math.pow(config.workersRoot, 2) - config.workersRoot)) {
      for (i <- 0 until config.gridSize; j <- 0 to 1) {
        gridArray(xOffset + config.gridSize + j)(yOffset + i) match {
          case Obstacle => grid.cells(config.gridSize - 1)(i) = Obstacle
          case _ =>
        }
      }
    }
  }


  private def calculateXOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    if (workerId.value <= workersRoot){
      0
    } else {
      var value = workerId.value
      var counter = 0
      while (value > workersRoot) {
        value -= workersRoot
        counter += 1
      }
      counter * gridSize
    }
  }

  private def calculateYOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    Math.floor((workerId.value - 1) % workersRoot).toInt * gridSize
  }
}
