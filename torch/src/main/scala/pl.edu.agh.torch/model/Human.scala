package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}


final case class HumanCell(smell: SmellArray, crowd : List[HumanCell]) extends SmellingCell {

  override type Self = HumanCell

  override def withSmell(smell: SmellArray): HumanCell = copy(smell = smell)
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(crowd : List[HumanCell]): T
}
object HumanAccessible {

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): HumanAccessible[HumanCell] =
    (crowd) => HumanCell(arg.smellWith(config.humanInitialSignal), crowd)

  def unapply(arg: EscapeCell)(implicit config: TorchConfig): HumanAccessible[EscapeCell] =
    (_) => EscapeCell(arg.smell)

  def unapply(arg: BufferCell)(implicit config: TorchConfig): HumanAccessible[BufferCell] =
    crowd => BufferCell(HumanCell(arg.smellWith(config.humanInitialSignal), crowd))

  def unapply(arg: GridPart)(implicit config: TorchConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
