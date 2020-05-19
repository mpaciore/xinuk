package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{EmptyCell, GridPart, SmellingCell}


final case class FireCell(smell: SmellMap) extends SmellingCell {
  override type Self = FireCell

  override def withSmell(smell: SmellMap): FireCell = copy(smell = smell)
}

trait FireAccessible[+T <: GridPart] {
  def withFire(): T
}

object FireAccessible {

  def unapply(arg: GridPart)(implicit config: TorchConfig): Option[FireAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: EscapeCell => Some(unapply(cell))
    case cell: HumanCell => Some(unapply(cell))
    case _ => None
  }

  def unapply(arg: EmptyCell)(implicit config: TorchConfig): FireAccessible[FireCell] =
    new FireAccessible[FireCell] {
      override def withFire(): FireCell = FireCell(arg.smellWith(config.fireInitialSignal))
    }

  def unapply(arg: HumanCell)(implicit config: TorchConfig): FireAccessible[FireCell] =
    new FireAccessible[FireCell] {
      override def withFire(): FireCell = FireCell(arg.smellWith(config.fireInitialSignal))
    }

  def unapply(arg: EscapeCell)(implicit config: TorchConfig): FireAccessible[FireCell] =
    new FireAccessible[FireCell] {
      override def withFire(): FireCell = FireCell(arg.smellWith(config.fireInitialSignal))
    }
}
