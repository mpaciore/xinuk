package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.EnhancedCell.NeighbourMap

sealed trait EnhancedCell {
  type Self <: EnhancedCell

  def cell: Cell = EmptyCell.Instance

  def withCell(newCell: Cell): Self

  def neighbours: NeighbourMap = Map.empty
}

object EnhancedCell {
  type NeighbourMap = Map[Direction, (Int, Int)]
}

final case class LocalEnhancedCell(override val cell: Cell,
                                   override val neighbours: NeighbourMap
                             ) extends EnhancedCell {
  override type Self = LocalEnhancedCell

  override def withCell(newCell: Cell): LocalEnhancedCell = {
    LocalEnhancedCell(newCell, neighbours)
  }
}

object LocalEnhancedCell {
  def apply(cell: Cell, neighbours: NeighbourMap): LocalEnhancedCell = new LocalEnhancedCell(cell, neighbours)
}

final case class RemoteEnhancedCell(override val cell: Cell,
                                    override val neighbours: NeighbourMap,
                                    workerId: WorkerId,
                                    targetCoordinates: (Int, Int)
                                   ) extends EnhancedCell {
  override type Self = RemoteEnhancedCell

  override def withCell(newCell: Cell): RemoteEnhancedCell = {
    RemoteEnhancedCell(newCell, neighbours, workerId, targetCoordinates)
  }

  def empty(): RemoteEnhancedCell = {
    RemoteEnhancedCell(neighbours, workerId, targetCoordinates)
  }
}

object RemoteEnhancedCell {
  def apply(neighbours: NeighbourMap, workerId: WorkerId, targetCoordinates: (Int, Int)): RemoteEnhancedCell =
    RemoteEnhancedCell(EmptyCell.Instance, neighbours, workerId, targetCoordinates)
  def apply(cell: Cell, neighbours: NeighbourMap, workerId: WorkerId, targetCoordinates: (Int, Int)): RemoteEnhancedCell =
    new RemoteEnhancedCell(cell, neighbours, workerId, targetCoordinates)
}

object Direction {
  sealed abstract class Direction(private val xShift: Int, private val yShift: Int) {
    def of(x: Int, y: Int): (Int, Int) = (x + xShift, y + yShift)
    def opposite: Direction = Direction.opposite(this)
    def adjacent: Set[Direction] = Direction.adjacent(this)
    def withAdjacent: Set[Direction] = adjacent + this
  }
  case object Top extends Direction(-1, 0)
  case object TopRight extends Direction(-1, 1)
  case object Right extends Direction(0, 1)
  case object BottomRight extends Direction(1, 1)
  case object Bottom extends Direction(1, 0)
  case object BottomLeft extends Direction(1, -1)
  case object Left extends Direction(0, -1)
  case object TopLeft extends Direction(-1, -1)

  def values: Seq[Direction] = Seq(Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Left, TopLeft)

  private def opposite(direction: Direction): Direction = direction match {
    case Top => Bottom
    case TopRight => BottomLeft
    case Right => Left
    case BottomRight => TopLeft
    case Bottom => Top
    case BottomLeft => TopRight
    case Left => Right
    case TopLeft => BottomRight
  }

  private def adjacent(direction: Direction): Set[Direction] = {
    val idx: Int = values.indexOf(direction)
    Set(values((idx - 1 + values.size) % values.size), values((idx + 1) % values.size))
  }
}
