package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.model.CellId

case class GridCellId(x: Int, y: Int) extends CellId

object GridCellId {
  def distance(id1: GridCellId, id2: GridCellId): Int = math.max(math.abs(id1.x - id2.x), math.abs(id1.y - id2.y))
}
