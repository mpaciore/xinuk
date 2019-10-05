package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.model.{SimulationMap, Tile, TileType}

object SimpleJsonMapGenerator {
  def emptyMap(size: Int = 32, mapTitle: String = "Empty map"): SimulationMap = {
    val tiles = List.tabulate(size, size) { case (x, y) => Tile(x, y, TileType.Empty) }.flatten
    SimulationMap(size, mapTitle, tiles)
  }

  def randomMap(size: Int = 32, obstaclesPercentage: Double = 5.0, title: String = "Random map"): SimulationMap = {
    val r = scala.util.Random
    val seed: Int = Math.floor(100 / obstaclesPercentage).toInt
    val tiles = List.tabulate(size, size) { case (x, y) => r.nextInt(seed) match {
      case 0 => Tile(x, y, TileType.Obstacle)
      case _ => Tile(x, y, TileType.Empty)
    }
    }.flatten
    SimulationMap(size, title, tiles)
  }

}
