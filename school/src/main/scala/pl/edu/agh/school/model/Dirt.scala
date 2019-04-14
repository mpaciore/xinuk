package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class DirtCell(energy: Energy, smell: SmellArray, lifespan: Long, signalIndex: Int) extends SmellingCell {
  override type Self = DirtCell

  override def withSmell(smell: SmellArray): DirtCell = copy(smell = smell)
}

trait DirtAccessible[+T <: GridPart] {
  def withDirt(energy: Energy, lifespan: Long): T
}

object DirtAccessible {

  def unapply(arg: StudentCell)(implicit config: SchoolConfig): DirtAccessible[DirtCell] =
    new DirtAccessible[DirtCell] {
      override def withDirt(energy: Energy, lifespan: Long): DirtCell = DirtCell(energy + config.algaeEnergeticCapacity, arg.smellWith(config.dirtInitialSignal.toSignalVector), lifespan, config.dirtSignalIndex)
    }

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): DirtAccessible[DirtCell] =
    new DirtAccessible[DirtCell] {
      override def withDirt(energy: Energy, lifespan: Long): DirtCell = DirtCell(energy, arg.smellWith(config.dirtInitialSignal.toSignalVector), lifespan, config.dirtSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): DirtAccessible[BufferCell] =
    new DirtAccessible[BufferCell] {
      override def withDirt(energy: Energy, lifespan: Long): BufferCell = BufferCell(DirtCell(energy, arg.smellWith(config.dirtInitialSignal.toSignalVector), lifespan, config.dirtSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[DirtAccessible[GridPart]] = arg match {
    case cell: StudentCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
