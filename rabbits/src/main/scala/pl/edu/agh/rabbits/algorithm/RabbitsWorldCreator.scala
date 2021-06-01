package pl.edu.agh.rabbits.algorithm

import pl.edu.agh.rabbits.config.RabbitsConfig
import pl.edu.agh.rabbits.model.{Lettuce, Rabbit}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.{CellContents, CellState, WorldBuilder}
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}

import scala.util.Random

object RabbitsWorldCreator extends WorldCreator[RabbitsConfig] {

  private val random = new Random(System.nanoTime())

  override def prepareWorld()(implicit config: RabbitsConfig): WorldBuilder = {
    val worldBuilder = GridWorldBuilder().withGridConnections()

    for {
      x <- 0 until config.worldWidth
      y <- 0 until config.worldHeight
      if (random.nextDouble() < config.spawnChance)
    } {
      val contents: CellContents = if (random.nextDouble() < config.rabbitSpawnChance) {
        Rabbit(config.rabbitStartEnergy, 0)
      }
      else {
        Lettuce(0)
      }

      worldBuilder(GridCellId(x, y)) = CellState(contents)
    }

    worldBuilder
  }
}
