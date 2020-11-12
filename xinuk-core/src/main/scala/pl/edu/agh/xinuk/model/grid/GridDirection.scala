package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.model.Direction

sealed class GridDirection(private val xShift: Int, private val yShift: Int) extends Direction {
  def of(id: GridCellId): GridCellId = GridCellId(id.x + xShift, id.y + yShift)

  override def opposite: GridDirection = GridDirection.opposite(this)

  override def adjacent: Seq[GridDirection] = GridDirection.adjacent(this)
  
  def stepsFrom(other: GridDirection): Int = GridDirection.stepsBetween(this, other)
}

object GridDirection {
  private val oppositeMap: Map[GridDirection, GridDirection] = Map(
    Top -> Bottom,
    TopRight -> BottomLeft,
    Right -> Left,
    BottomRight -> TopLeft,
    Bottom -> Top,
    BottomLeft -> TopRight,
    Left -> Right,
    TopLeft -> BottomRight
  )

  private val adjacentMap: Map[GridDirection, Seq[GridDirection]] = values.map { direction =>
    val idx: Int = values.indexOf(direction)
    (direction, Seq(values((idx - 1 + values.size) % values.size), values((idx + 1) % values.size)))
  }.toMap

  private val distanceMap: Map[GridDirection, Map[GridDirection, Int]] = values.map { direction =>
    val idx = values.indexOf(direction)
    (direction, values.map { otherDirection =>
      val otherIdx = values.indexOf(otherDirection)
      val distance = math.min(math.abs(idx - otherIdx), values.size - math.abs(idx - otherIdx))
      (otherDirection, distance)
    }.toMap)
  }.toMap
  
  private def opposite(direction: GridDirection): GridDirection = oppositeMap(direction)

  private def adjacent(direction: GridDirection): Seq[GridDirection] = adjacentMap(direction)
  
  private def stepsBetween(direction: GridDirection, other: GridDirection): Int = distanceMap(direction)(other)
  
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