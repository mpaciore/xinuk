package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.GridPart.SmellMap
import pl.edu.agh.xinuk.model._

final case class ForaminiferaCell(energy: Energy, signal: SmellMap, lifespan: Long) extends GridPart {
  override type Self = ForaminiferaCell

  override def withSmell(smell: SmellMap): ForaminiferaCell = copy(signal = smell)
}

trait ForaminiferaAccessible[+T <: GridPart] {
  def withForaminifera(energy: Energy, lifespan: Long): T
}

object ForaminiferaAccessible {

  def unapply(arg: AlgaeCell)(implicit config: ForminConfig): ForaminiferaAccessible[ForaminiferaCell] =
    new ForaminiferaAccessible[ForaminiferaCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): ForaminiferaCell = ForaminiferaCell(energy + config.algaeEnergeticCapacity, arg.signalWith(config.foraminiferaInitialSignal), lifespan)
    }

  def unapply(arg: EmptyCell)(implicit config: ForminConfig): ForaminiferaAccessible[ForaminiferaCell] =
    new ForaminiferaAccessible[ForaminiferaCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): ForaminiferaCell = ForaminiferaCell(energy, arg.signalWith(config.foraminiferaInitialSignal), lifespan)
    }

  def unapply(arg: BufferCell)(implicit config: ForminConfig): ForaminiferaAccessible[BufferCell] =
    new ForaminiferaAccessible[BufferCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): BufferCell = BufferCell(ForaminiferaCell(energy, arg.signalWith(config.foraminiferaInitialSignal), lifespan))
    }

  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[ForaminiferaAccessible[GridPart]] = arg match {
    case cell: AlgaeCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
