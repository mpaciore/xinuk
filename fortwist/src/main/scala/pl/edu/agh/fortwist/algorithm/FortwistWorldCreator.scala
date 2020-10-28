package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.{Foraminifera, Seabed}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.{CellState, WorldBuilder}
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}

import scala.util.Random

object FortwistWorldCreator extends WorldCreator[FortwistConfig] {

  private val random = new Random(System.nanoTime())

  override def prepareWorld()(implicit config: FortwistConfig): WorldBuilder = {
    val worldBuilder = GridWorldBuilder().withGridConnections()

    for {
      x <- 0 until config.worldWidth
      y <- 0 until config.worldHeight
    } {
      val foraminiferas: Seq[Foraminifera] = if (random.nextDouble() < config.foraminiferaSpawnChance) {
        Seq(Foraminifera())
      } else {
        Seq()
      }
      worldBuilder(GridCellId(x, y)) = CellState(Seabed(foraminiferas, config.algaeStartEnergy))
    }

    worldBuilder
  }
}
