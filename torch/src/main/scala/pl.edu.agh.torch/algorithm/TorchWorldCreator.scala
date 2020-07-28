package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{Exit, Fire, Person}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}
import pl.edu.agh.xinuk.model.{CellContents, CellState, WorldBuilder}

import scala.util.Random

object TorchWorldCreator extends WorldCreator[TorchConfig] {

  private val random = new Random(System.nanoTime())

  override def prepareWorld()(implicit config: TorchConfig): WorldBuilder = {
    val worldBuilder = GridWorldBuilder().withGridConnections()

    for {
      x <- 0 until config.worldSize
      y <- 0 until config.worldSize
      if (random.nextDouble() < config.spawnChance)
    } {
      val contents: Option[CellContents] = random.nextInt(3) match {
        case 0 if (random.nextDouble() < config.personSpawnChance) =>
          val speed = random.nextInt(config.personMaxSpeed) + 1
          Some(Person(speed))

        case 1 if (random.nextDouble() < config.exitSpawnChance) =>
          Some(Exit)

        case 2 if (random.nextDouble() < config.fireSpawnChance) =>
          Some(Fire)

        case _ =>
          None
      }

      contents.foreach(c => worldBuilder(GridCellId(x, y)) = CellState(c))
    }

    worldBuilder
  }
}
