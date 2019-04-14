package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class CleanerCell(energy: Energy, smell: SmellArray, lifespan: Long, signalIndex: Int) extends SmellingCell {
  override type Self = CleanerCell

  override def withSmell(smell: SmellArray): CleanerCell = copy(smell = smell)
}

trait CleanerAccessible[+T <: GridPart] {
  def withCleaner(energy: Energy, lifespan: Long): T
}

object CleanerAccessible {

  def unapply(arg: DirtCell)(implicit config: SchoolConfig): CleanerAccessible[CleanerCell] =
    new CleanerAccessible[CleanerCell] {
      override def withCleaner(energy: Energy, lifespan: Long): CleanerCell = CleanerCell(energy + config.algaeEnergeticCapacity, arg.smellWith(config.cleanerInitialSignal.toSignalVector), lifespan, config.cleanerInitialSignalIndex)
    }

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): CleanerAccessible[CleanerCell] =
    new CleanerAccessible[CleanerCell] {
      override def withCleaner(energy: Energy, lifespan: Long): CleanerCell = CleanerCell(energy, arg.smellWith(config.cleanerInitialSignal.toSignalVector), lifespan, config.cleanerInitialSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): CleanerAccessible[BufferCell] =
    new CleanerAccessible[BufferCell] {
      override def withCleaner(energy: Energy, lifespan: Long): BufferCell = BufferCell(CleanerCell(energy, arg.smellWith(config.cleanerInitialSignal.toSignalVector), lifespan, config.cleanerInitialSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[CleanerAccessible[GridPart]] = arg match {
    case cell: DirtCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
