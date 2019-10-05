package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.model.{SimulationMap, Tile, TileType}

object SimpleJsonMapGenerator {
  def emptyMap(size: Int = 32): SimulationMap = {
    val title = "Example map"
    val tiles = List.tabulate(size, size) { case (x, y) => Tile(x, y, TileType.Empty) }.flatten
    val simulationMap = SimulationMap(size, title, tiles)
    simulationMap
  }

  def randomMap(size: Int = 32, title: String = "Random map"): SimulationMap = {
    val r = scala.util.Random

    val tiles = List.tabulate(size, size) { case (x, y) => r.nextInt(4) match {
      case 0 => Tile(x, y, TileType.Obstacle)
      case _ => Tile(x, y, TileType.Empty)
    }
    }.flatten
    val simulationMap = SimulationMap(size, title, tiles)

    simulationMap
  }

}
