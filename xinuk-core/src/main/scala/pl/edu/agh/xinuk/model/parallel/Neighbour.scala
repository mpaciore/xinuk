package pl.edu.agh.xinuk.model.parallel

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.{Opt, SealedEnumCompanion}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.WorkerId

import scala.collection.immutable.TreeSet


sealed trait NeighbourPosition {

  def neighbourId(of: WorkerId)(implicit config: XinukConfig): Opt[WorkerId]

  def bufferZone(implicit config: XinukConfig): TreeSet[(Int, Int)]

  protected[parallel] def bufferZoneAffectedModifier: (Int, Int)

  def affectedCells(implicit config: XinukConfig): Iterator[(Int, Int)] = {
    val (xModifier, yModifier) = bufferZoneAffectedModifier
    bufferZone.iterator.map { case (x, y) => (x + xModifier, y + yModifier) }
  }

}

sealed protected abstract class NeighbourPositionGen(idModifier: XinukConfig => Int)(gridEdgeRangeToZone: Range => Iterator[(Int, Int)])
  (protected[parallel] val bufferZoneAffectedModifier: (Int, Int))
  extends NeighbourPosition {

  private def isValid(id: WorkerId)(implicit config: XinukConfig): Boolean = {
    (id.value > 0) && (id.value <= math.pow(config.workersRoot, 2))
  }

  override def neighbourId(of: WorkerId)(implicit config: XinukConfig): Opt[WorkerId] = {
    val modifier = idModifier(config)
    val newValue = of.value + modifier

    def canSkipRow = modifier % config.workersRoot == 0

    def isSameRow = (newValue - 1) / config.workersRoot == (of.value - 1) / config.workersRoot

    of.opt
      .filter(_ => canSkipRow || isSameRow)
      .map(_.copy(of.value + idModifier(config)))
      .filter(isValid)
  }

  override def bufferZone(implicit config: XinukConfig): TreeSet[(Int, Int)] = {
    gridEdgeRangeToZone(1 until config.gridSize - 1).to[TreeSet]
  }

  def extendedZone(implicit config: XinukConfig): TreeSet[(Int, Int)] = {
    val (xMod, yMod) = bufferZoneAffectedModifier
    gridEdgeRangeToZone(0 until config.gridSize).map {
      case (x, y) if xMod < 0 => (x + xMod, y)
      case (x, y) if yMod < 0 => (x, y + yMod)
      case other => other
    }.to[TreeSet]
  }

}

sealed protected abstract class NeighbourPositionComposite(pos1: NeighbourPositionGen, pos2: NeighbourPositionGen)
  extends NeighbourPosition {

  override protected[parallel] val bufferZoneAffectedModifier: (Int, Int) = {
    val (x1, y1) = pos1.bufferZoneAffectedModifier
    val (x2, y2) = pos2.bufferZoneAffectedModifier
    (x1 + x2, y1 + y2)
  }

  override def neighbourId(of: WorkerId)(implicit config: XinukConfig): Opt[WorkerId] = {
    pos1.neighbourId(of).flatMap(pos2.neighbourId)
  }

  override def bufferZone(implicit config: XinukConfig): TreeSet[(Int, Int)] = {
    pos1.extendedZone & pos2.extendedZone
  }
}

object NeighbourPosition extends SealedEnumCompanion[NeighbourPosition] {

  case object Top extends NeighbourPositionGen(-_.workersRoot)(_.iterator.map((0, _)))((1, 0))

  case object Bottom extends NeighbourPositionGen(_.workersRoot)(range => range.iterator.map((range.end, _)))((-1, 0))

  case object Right extends NeighbourPositionGen(_ => 1)(range => range.iterator.map((_, range.end)))((0, -1))

  case object Left extends NeighbourPositionGen(_ => -1)(_.iterator.map((_, 0)))((0, 1))

  case object TopRight extends NeighbourPositionComposite(Top, Right)

  case object TopLeft extends NeighbourPositionComposite(Top, Left)

  case object BottomRight extends NeighbourPositionComposite(Bottom, Right)

  case object BottomLeft extends NeighbourPositionComposite(Bottom, Left)

  override val values: List[NeighbourPosition] = caseObjects
}

final case class Neighbour(position: NeighbourPosition) extends AnyVal