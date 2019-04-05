package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class HumanCell(smell: SmellArray, crowd : List[HumanCell], speed : Int, pursuedSignalIndex: Int) (implicit config: TorchConfig) extends SmellingCell {

  override type Self = HumanCell

  override def withSmell(smell: SmellArray): HumanCell = copy(smell = smell)
}

trait HumanAccessible[+T <: GridPart] {
  def withHuman(crowd : List[HumanCell], speed : Int): T
}
object HumanAccessible {

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): HumanAccessible[HumanCell] =
    new HumanAccessible[HumanCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): HumanCell = HumanCell(arg.smellWith(config.humanInitialSignal.toSignalVector), crowd, speed, config.humanPursuedSignalIndex)
    }

  def unapply(arg: EscapeCell): HumanAccessible[EscapeCell] =
    new HumanAccessible[EscapeCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): EscapeCell = EscapeCell(arg.smell)
    }

  def unapply(arg: BufferCell)(implicit config: TorchConfig): HumanAccessible[BufferCell] =
    new HumanAccessible[BufferCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): BufferCell = BufferCell(HumanCell(arg.smellWith(config.humanInitialSignal.toSignalVector), crowd, speed, config.humanPursuedSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: TorchConfig): Option[HumanAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
