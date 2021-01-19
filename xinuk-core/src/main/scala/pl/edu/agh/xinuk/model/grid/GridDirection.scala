package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.model.Direction

sealed class GridDirection(private val xShift: Int, private val yShift: Int) extends Direction {
  def of(id: GridCellId): GridCellId = GridCellId(id.x + xShift, id.y + yShift)

  override def opposite: GridDirection = GridDirection.opposite(this)

  override def adjacent: Seq[GridDirection] = GridDirection.adjacent(this)

  def isDiagonal: Boolean = GridDirection.isDiagonal(this)

  def stepsFrom(other: GridDirection): Int = GridDirection.stepsBetween(this, other)

  def clockwise: GridDirection = GridDirection.clockwise(this)

  def counterClockwise: GridDirection = GridDirection.counterClockwise(this)
}

object GridDirection {

  implicit val ordering: Ordering[GridDirection] = Ordering.by(values.indexOf(_))

  private val valuesSeq: Seq[GridDirection] = Seq(Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left, TopLeft)

  implicit def values: Seq[GridDirection] = valuesSeq

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
    (direction, Seq(clockwise(direction), counterClockwise(direction)))
  }.toMap

  private val distanceMap: Map[GridDirection, Map[GridDirection, Int]] = values.map { direction =>
    val idx = values.indexOf(direction)
    (direction, values.map { otherDirection =>
      val otherIdx = values.indexOf(otherDirection)
      val distance = math.min(math.abs(idx - otherIdx), values.size - math.abs(idx - otherIdx))
      (otherDirection, distance)
    }.toMap)
  }.toMap

  private val diagonals: Set[GridDirection] = Set(TopRight, BottomRight, BottomLeft, TopLeft)

  private def opposite(direction: GridDirection): GridDirection = oppositeMap(direction)

  private def adjacent(direction: GridDirection): Seq[GridDirection] = adjacentMap(direction)

  private def isDiagonal(direction: GridDirection): Boolean = diagonals.contains(direction)

  private def stepsBetween(direction: GridDirection, other: GridDirection): Int = distanceMap(direction)(other)

  private def clockwise(direction: GridDirection): GridDirection = {
    val idx: Int = values.indexOf(direction)
    values((idx + 1) % values.size)
  }

  private def counterClockwise(direction: GridDirection): GridDirection = {
    val idx: Int = values.indexOf(direction)
    values((idx - 1 + values.size) % values.size)
  }

  case object Top extends GridDirection(-1, 0)

  case object TopRight extends GridDirection(-1, 1)

  case object Right extends GridDirection(0, 1)

  case object BottomRight extends GridDirection(1, 1)

  case object Bottom extends GridDirection(1, 0)

  case object BottomLeft extends GridDirection(1, -1)

  case object Left extends GridDirection(0, -1)

  case object TopLeft extends GridDirection(-1, -1)

}