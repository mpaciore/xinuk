package pl.edu.agh.formin.model.parallel

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.{Opt, SealedEnumCompanion}
import pl.edu.agh.formin.WorkerId
import pl.edu.agh.formin.config.ForminConfig

import scala.collection.immutable.TreeSet


sealed trait NeighbourPosition {

  def neighbourId(of: WorkerId)(implicit config: ForminConfig): Opt[WorkerId]

  def bufferZone(implicit config: ForminConfig): TreeSet[(Int, Int)]

  protected[parallel] def bufferZoneAffectedModifier: (Int, Int)

  def affectedCells(implicit config: ForminConfig): Iterator[(Int, Int)] = {
    val (xModifier, yModifier) = bufferZoneAffectedModifier
    bufferZone.iterator.map { case (x, y) => (x + xModifier, y + yModifier) }
  }

}

sealed protected abstract class NeighbourPositionGen(idModifier: ForminConfig => Int)(gridEdgeRangeToZone: Range => Iterator[(Int, Int)])
                                                    (protected[parallel] val bufferZoneAffectedModifier: (Int, Int))
  extends NeighbourPosition {


  override def neighbourId(of: WorkerId)(implicit config: ForminConfig): Opt[WorkerId] = {
    val modifier = idModifier(config)
    val newValue = of.value + modifier

    def canSkipRow = modifier % config.workersRoot == 0

    def isSameRow = (newValue - 1) / config.workersRoot == (of.value - 1) / config.workersRoot

    of.opt
      .filter(_ => canSkipRow || isSameRow)
      .map(_.copy(of.value + idModifier(config)))
      .filter(_.isValid)
  }

  override def bufferZone(implicit config: ForminConfig): TreeSet[(Int, Int)] = {
    gridEdgeRangeToZone(0 until config.gridSize).to[TreeSet]
  }

}

sealed protected abstract class NeighbourPositionComposite(pos1: NeighbourPositionGen, pos2: NeighbourPositionGen)
  extends NeighbourPosition {

  override protected[parallel] val bufferZoneAffectedModifier: (Int, Int) = {
    val (x1, y1) = (pos1: NeighbourPosition).bufferZoneAffectedModifier
    val (x2, y2) = (pos2: NeighbourPosition).bufferZoneAffectedModifier
    (x1 + x2, y1 + y2)
  }

  override def neighbourId(of: WorkerId)(implicit config: ForminConfig): Opt[WorkerId] = {
    pos1.neighbourId(of).flatMap(pos2.neighbourId)
  }

  override def bufferZone(implicit config: ForminConfig): TreeSet[(Int, Int)] = {
    pos1.bufferZone & pos2.bufferZone
  }
}

object NeighbourPosition extends SealedEnumCompanion[NeighbourPosition] {

  case object Top extends NeighbourPositionGen(-_.workersRoot)(_.iterator.map((0, _)))((1, 0))

  case object Bottom extends NeighbourPositionGen(_.workersRoot)(range => range.iterator.map((range.end - 1, _)))((-1, 0))

  case object Right extends NeighbourPositionGen(_ => 1)(range => range.iterator.map((_, range.end - 1)))((0, -1))

  case object Left extends NeighbourPositionGen(_ => -1)(_.iterator.map((_, 0)))((0, 1))

  case object TopRight extends NeighbourPositionComposite(Top, Right)

  case object TopLeft extends NeighbourPositionComposite(Top, Left)

  case object BottomRight extends NeighbourPositionComposite(Bottom, Right)

  case object BottomLeft extends NeighbourPositionComposite(Bottom, Left)

  override val values: List[NeighbourPosition] = caseObjects
}

final case class Neighbour(position: NeighbourPosition) extends AnyVal