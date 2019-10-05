package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, Obstacle}

final case class SimulationMap(size: Int, title: String, tiles: List[Tile]) {

  def getTilesAsArray: Array[Array[GridPart]] = {
    val array: Array[Array[GridPart]] = Array.ofDim[GridPart](size, size)

    tiles.foreach(tile => array(tile.row)(tile.column) = tile.tileType match {
      case TileType.Empty => EmptyCell.Instance
      case TileType.Obstacle => Obstacle
    })

    array
  }
}
