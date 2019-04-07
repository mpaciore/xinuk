package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class TeacherCell(energy: Energy, smell: SmellArray, lifespan: Long, signalIndex: Int) extends SmellingCell {
  override type Self = TeacherCell

  override def withSmell(smell: SmellArray): TeacherCell = copy(smell = smell)
}

trait TeacherAccessible[+T <: GridPart] {
  def withTeacher(energy: Energy, lifespan: Long): T
}

object TeacherAccessible {

  def unapply(arg: StudentCell)(implicit config: SchoolConfig): TeacherAccessible[TeacherCell] =
    new TeacherAccessible[TeacherCell] {
      override def withTeacher(energy: Energy, lifespan: Long): TeacherCell = TeacherCell(energy + config.algaeEnergeticCapacity, arg.smellWith(config.teacherInitialSignal.toSignalVector), lifespan, config.teacherSignalIndex)
    }

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): TeacherAccessible[TeacherCell] =
    new TeacherAccessible[TeacherCell] {
      override def withTeacher(energy: Energy, lifespan: Long): TeacherCell = TeacherCell(energy, arg.smellWith(config.teacherInitialSignal.toSignalVector), lifespan, config.teacherSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): TeacherAccessible[BufferCell] =
    new TeacherAccessible[BufferCell] {
      override def withTeacher(energy: Energy, lifespan: Long): BufferCell = BufferCell(TeacherCell(energy, arg.smellWith(config.teacherInitialSignal.toSignalVector), lifespan, config.teacherSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[TeacherAccessible[GridPart]] = arg match {
    case cell: StudentCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
