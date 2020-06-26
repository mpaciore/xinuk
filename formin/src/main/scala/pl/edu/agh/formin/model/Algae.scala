package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Cell}

final case class AlgaeCell(smell: SmellMap, lifespan: Long) extends Cell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellMap): AlgaeCell = copy(smell = smell)
}

trait AlgaeAccessible[+T <: Cell] {
  def withAlgae(lifespan: Long): T
}

object AlgaeAccessible {

  def unapply(arg: EmptyCell)(implicit config: ForminConfig): AlgaeAccessible[AlgaeCell] =
    new AlgaeAccessible[AlgaeCell] {
      override def withAlgae(lifespan: Long): AlgaeCell = AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan)
    }

  def unapply(arg: BufferCell)(implicit config: ForminConfig): AlgaeAccessible[BufferCell] =
    new AlgaeAccessible[BufferCell] {
      override def withAlgae(lifespan: Long): BufferCell = BufferCell(AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan))
    }

  def unapply(arg: Cell)(implicit config: ForminConfig): Option[AlgaeAccessible[Cell]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
