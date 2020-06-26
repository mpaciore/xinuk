package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{EmptyCell, Cell}

final case class HumanCell(smell: SmellMap, crowd: List[HumanCell], speed: Int)(implicit config: TorchConfig) extends Cell {

  override type Self = HumanCell

  override def withSmell(smell: SmellMap): HumanCell = copy(smell = smell)
}

trait HumanAccessible[+T <: Cell] {
  def withHuman(crowd: List[HumanCell], speed: Int): T
}

object HumanAccessible {

  def unapply(arg: Cell)(implicit config: TorchConfig): Option[HumanAccessible[Cell]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case _ => None
  }

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): HumanAccessible[HumanCell] =
    new HumanAccessible[HumanCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): HumanCell = HumanCell(arg.smellWith(config.humanInitialSignal), crowd, speed)
    }

  def unapply(arg: EscapeCell): HumanAccessible[EscapeCell] =
    new HumanAccessible[EscapeCell] {
      override def withHuman(crowd: List[HumanCell], speed: Int): EscapeCell = EscapeCell(arg.smell)
    }
}
