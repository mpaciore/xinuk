package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{EmptyCell, Cell}

final case class EscapeCell(smell: SmellMap) extends Cell {
  override type Self = EscapeCell

  override def withSmell(smell: SmellMap): EscapeCell = copy(smell = smell)
}

trait EscapeAccessible[+T <: Cell] {
  def withEscape(): T
}

object EscapeAccessible {

  def unapply(arg: Cell)(implicit config: TorchConfig): Option[EscapeAccessible[Cell]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): EscapeAccessible[EscapeCell] =
    new EscapeAccessible[EscapeCell] {
      override def withEscape(): EscapeCell = EscapeCell(arg.smellWith(config.escapeInitialSignal))
    }
}
