package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}


final case class HumanCell(smell: SmellArray) extends SmellingCell {
  override type Self = HumanCell

  override def withSmell(smell: SmellArray): HumanCell = copy(smell = smell)
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(): T
}
object HumanAccessible {

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): HumanAccessible[HumanCell] =
    () => HumanCell(arg.smellWith(config.humanInitialSignal))

  def unapply(arg: BufferCell)(implicit config: TorchConfig): HumanAccessible[BufferCell] =
    () => BufferCell(HumanCell(arg.smellWith(config.humanInitialSignal)))

  def unapply(arg: GridPart)(implicit config: TorchConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
