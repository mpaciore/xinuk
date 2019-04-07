package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class CleanerCell(energy: Energy, smell: SmellArray, lifespan: Long, pursuedSignalIndex: Int) extends SmellingCell {
  override type Self = CleanerCell

  override def withSmell(smell: SmellArray): CleanerCell = copy(smell = smell)
}

trait ForaminiferaAccessible[+T <: GridPart] {
  def withForaminifera(energy: Energy, lifespan: Long): T
}

object ForaminiferaAccessible {

  def unapply(arg: StudentCell)(implicit config: SchoolConfig): ForaminiferaAccessible[CleanerCell] =
    new ForaminiferaAccessible[CleanerCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): CleanerCell = CleanerCell(energy + config.algaeEnergeticCapacity, arg.smellWith(config.cleanerInitialSignal.toSignalVector), lifespan, config.cleanerInitialSignalIndex)
    }

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): ForaminiferaAccessible[CleanerCell] =
    new ForaminiferaAccessible[CleanerCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): CleanerCell = CleanerCell(energy, arg.smellWith(config.cleanerInitialSignal.toSignalVector), lifespan, config.cleanerInitialSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): ForaminiferaAccessible[BufferCell] =
    new ForaminiferaAccessible[BufferCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): BufferCell = BufferCell(CleanerCell(energy, arg.smellWith(config.cleanerInitialSignal.toSignalVector), lifespan, config.cleanerInitialSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[ForaminiferaAccessible[GridPart]] = arg match {
    case cell: StudentCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
