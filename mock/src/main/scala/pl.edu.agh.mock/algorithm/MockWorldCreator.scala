package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}

object MockWorldCreator extends WorldCreator[MockConfig] {

  override def prepareWorld()(implicit config: MockConfig): GridWorldBuilder = {
    val worldBuilder: GridWorldBuilder = GridWorldBuilder().withGridConnections()//.withWrappedBoundaries()

    makeGrid(1, worldBuilder)

    worldBuilder
  }

  private def makeGrid(n: Int, worldBuilder: GridWorldBuilder)(implicit config: MockConfig): Unit = {
    val xUnit = config.worldWidth / (n + 1)
    val yUnit = config.worldHeight / (n + 1)
    for {
      xIdx <- 1 to n
      yIdx <- 1 to n
    } {
      worldBuilder(GridCellId(xIdx * xUnit, yIdx * yUnit)) = CellState(Mock)
    }
  }
}
