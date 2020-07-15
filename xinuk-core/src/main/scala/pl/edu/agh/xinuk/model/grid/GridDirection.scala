package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.model.Direction

sealed class GridDirection(private val xShift: Int, private val yShift: Int) extends Direction {
  def of(id: GridCellId): GridCellId = GridCellId(id.x + xShift, id.y + yShift)

  def opposite: GridDirection = GridDirection.opposite(this)

  def adjacent: Seq[GridDirection] = GridDirection.adjacent(this)
}

object GridDirection {

  private def opposite(direction: GridDirection): GridDirection = direction match {
    case Top => Bottom
    case TopRight => BottomLeft
    case Right => Left
    case BottomRight => TopLeft
    case Bottom => Top
    case BottomLeft => TopRight
    case Left => Right
    case TopLeft => BottomRight
    case _ => throw new UnsupportedOperationException("Cannot get GridDirection opposite to non-GridDirection.")
  }

  private def adjacent(direction: GridDirection): Seq[GridDirection] = {
    val idx: Int = values.indexOf(direction)
    Seq(values((idx - 1 + values.size) % values.size), values((idx + 1) % values.size))
  }

  implicit def values: Seq[GridDirection] = Seq(Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left, TopLeft)

  case object Top extends GridDirection(-1, 0)

  case object TopRight extends GridDirection(-1, 1)

  case object Right extends GridDirection(0, 1)

  case object BottomRight extends GridDirection(1, 1)

  case object Bottom extends GridDirection(1, 0)

  case object BottomLeft extends GridDirection(1, -1)

  case object Left extends GridDirection(0, -1)

  case object TopLeft extends GridDirection(-1, -1)

}