package pl.edu.agh.mock.utlis

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.model.{SimulationMap, Tile}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{EmptyCell, Grid, GridPart, Obstacle, WorkerId}

object GridUtils extends LazyLogging{

  def addDataFromFile(filename: String, grid: Grid)(implicit config: XinukConfig): Unit = {
    val simulationMap: SimulationMap = JsonMapParser.parseMapFromJson(filename)
    val gridArray = simulationMap.getTilesAsArray
    val xOffset = calculateXOffset(grid.workerId, config.workersRoot, config.gridSize)
    val yOffset = calculateYOffset(grid.workerId, config.workersRoot, config.gridSize)

    for (i <- 0 until config.gridSize; j <- 0 until config.gridSize) {
      grid.cells(i)(j) match {
        case EmptyCell.Instance => grid.cells(i)(j) = gridArray(i + xOffset)(j + yOffset)
        case _ =>
      }
    }
  }

  private def calculateXOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    (workersRoot - 1 - workerId.value % workersRoot) * gridSize
  }

  private def calculateYOffset(workerId: WorkerId, workersRoot: Int, gridSize: Int): Int = {
    Math.floor((workerId.value - 1) / workersRoot).toInt * gridSize
  }
}
