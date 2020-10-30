package pl.edu.agh.urban.algorithm

import java.awt.Color

import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.urban.model.{SignalSourceCell, UrbanCell}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.CellState
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}

object UrbanWorldCreator extends WorldCreator[UrbanConfig] {


  override def prepareWorld()(implicit config: UrbanConfig): GridWorldBuilder = {
    val worldBuilder: GridWorldBuilder = GridWorldBuilder().withGridConnections()
    loadMap(worldBuilder)
    if (config.pathCreation != "None") {
      createSignalSources(worldBuilder)
    } else {
      prepareTargets(worldBuilder)
    }
    worldBuilder
  }

  private def loadMap(worldBuilder: GridWorldBuilder)(implicit config: UrbanConfig): Unit = {
    val img = config.mapImage
    for {
      x <- 0 until img.getWidth
      y <- 0 until img.getHeight
    } {
      val rgb = img.getRGB(x, y)
      val color = new Color(
        (rgb >> 16) & 0xFF,
        (rgb >> 8) & 0xFF,
        rgb & 0xFF
      )
      worldBuilder(GridCellId(x, y)) = CellState(UrbanCell(config.colorToTileType(color)))
    }
  }

  private def createSignalSources(worldBuilder: GridWorldBuilder)(implicit config: UrbanConfig): Unit = {
    val entranceIds: Set[GridCellId] = config.targets.find(_.id == config.pathCreation).get.entrances.map(_.gridId).toSet
    entranceIds.foreach { id =>
      worldBuilder(id).updateContents(SignalSourceCell())
    }
  }

  private def prepareTargets(worldBuilder: GridWorldBuilder)(implicit config: UrbanConfig): Unit = {
    config.targets.foreach {
      target => // create entrance instance with fraction of population according to the number of entrances
    }
  }
}
