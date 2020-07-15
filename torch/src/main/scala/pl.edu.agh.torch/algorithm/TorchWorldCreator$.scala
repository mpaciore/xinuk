package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{EscapeAccessible, FireAccessible, HumanAccessible}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.{EmptyCell, Grid, NonPlanarConnections}

import scala.util.Random

object TorchWorldCreator$ extends WorldCreator[TorchConfig] {

  private val random = new Random(System.nanoTime())

  override def buildWorld(implicit config: TorchConfig): (Grid, NonPlanarConnections) = {
    val grid = Grid.empty()
    var humanCount = 0L
    var fireCount = 0L
    var escapesCount = 0L
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grid.cells(x)(y) =
          random.nextInt(3) match {
            case 0 =>
              if (random.nextDouble() < config.humanSpawnChance) {
                humanCount += 1
                val speed = random.nextInt(config.humanMaxSpeed) + 1
                HumanAccessible.unapply(EmptyCell.Instance).withHuman(List.empty, speed)
              } else {
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.escapeSpawnChance) {
                escapesCount += 1
                EscapeAccessible.unapply(EmptyCell.Instance).withEscape()
              } else {
                grid.cells(x)(y)
              }
            case 2 =>
              if (random.nextDouble() < config.fireSpawnChance) {
                fireCount += 1
                FireAccessible.unapply(EmptyCell.Instance).withFire()
              } else {
                grid.cells(x)(y)
              }
          }
      }
    }

    (grid, NonPlanarConnections.empty)
  }
}
