package pl.edu.agh.formin.model.parallel

import akka.actor.ActorRef
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.{Opt, SealedEnumCompanion}
import pl.edu.agh.formin.WorkerId
import pl.edu.agh.formin.config.ForminConfig


sealed trait NeighbourPosition {
  def neighbourId(of: WorkerId)(implicit config: ForminConfig): Opt[WorkerId]

  def bufferZone(implicit config: ForminConfig): Set[(Int, Int)]
}

sealed protected abstract class NeighbourPositionGen(idModifier: ForminConfig => Int)(gridEdgeRangeToZone: Range => Iterator[(Int, Int)])
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

  override def bufferZone(implicit config: ForminConfig): Set[(Int, Int)] = {
    gridEdgeRangeToZone(0 until config.gridSize).toSet
  }
}

sealed protected abstract class NeighbourPositionComposite(pos1: NeighbourPositionGen, pos2: NeighbourPositionGen) extends NeighbourPosition {
  override def neighbourId(of: WorkerId)(implicit config: ForminConfig): Opt[WorkerId] = {
    pos1.neighbourId(of).flatMap(pos2.neighbourId)
  }

  override def bufferZone(implicit config: ForminConfig): Set[(Int, Int)] = {
    pos1.bufferZone & pos2.bufferZone
  }
}

object NeighbourPosition extends SealedEnumCompanion[NeighbourPosition] {

  case object Top extends NeighbourPositionGen(-_.workersRoot)(_.iterator.map((0, _)))

  case object Bottom extends NeighbourPositionGen(_.workersRoot)(range => range.iterator.map((range.end, _)))

  case object Right extends NeighbourPositionGen(_ => 1)(range => range.iterator.map((_, range.end)))

  case object Left extends NeighbourPositionGen(_ => -1)(_.iterator.map((_, 0)))

  case object TopRight extends NeighbourPositionComposite(Top, Right)

  case object TopLeft extends NeighbourPositionComposite(Top, Left)

  case object BottomRight extends NeighbourPositionComposite(Bottom, Right)

  case object BottomLeft extends NeighbourPositionComposite(Bottom, Left)

  override val values: List[NeighbourPosition] = caseObjects
}

final case class Neighbour(position: NeighbourPosition, ref: ActorRef)