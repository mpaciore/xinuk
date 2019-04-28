package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class StudentCell(smell: SmellArray, lifespan: Long, signalIndex: Int) extends SmellingCell {
  override type Self = StudentCell

  override def withSmell(smell: SmellArray): StudentCell = copy(smell = smell)
}

trait StudentAccessible[+T <: GridPart] {
  def withStudent(lifespan: Long): T
}

object StudentAccessible {

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): StudentAccessible[StudentCell] =
    new StudentAccessible[StudentCell] {
      override def withStudent(lifespan: Long): StudentCell = StudentCell(arg.smellWith(config.studentInitialSignal.toSignalVector), lifespan, config.studentSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): StudentAccessible[BufferCell] =
    new StudentAccessible[BufferCell] {
      override def withStudent(lifespan: Long): BufferCell = BufferCell(StudentCell(arg.smellWith(config.studentInitialSignal.toSignalVector), lifespan, config.studentSignalIndex))
    }

  def unapply(arg: DirtCell)(implicit config: SchoolConfig): StudentAccessible[StudentCell] =
    new StudentAccessible[StudentCell] {
      override def withStudent(lifespan: Long): StudentCell = StudentCell(arg.smellWith(config.studentInitialSignal.toSignalVector), lifespan, config.studentSignalIndex)
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[StudentAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
