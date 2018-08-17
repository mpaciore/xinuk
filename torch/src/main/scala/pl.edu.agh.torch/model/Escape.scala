package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, SmellingCell}

final case class EscapeCell(smell: SmellArray) extends SmellingCell {
  override type Self = EscapeCell

  override def withSmell(smell: SmellArray): EscapeCell = copy(smell = smell)
}

trait EscapeAccessible[+T <: GridPart] {
  def withEscape(): T
}
object EscapeAccessible {

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): EscapeAccessible[EscapeCell] =
    new EscapeAccessible[EscapeCell] {
      override def withEscape(): EscapeCell = EscapeCell(arg.smellWith(config.escapeInitialSignal))
    }

  def unapply(arg: GridPart)(implicit config: TorchConfig): Option[EscapeAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case _ => None
  }
}
