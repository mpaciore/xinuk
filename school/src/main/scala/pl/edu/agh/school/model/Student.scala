package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class StudentCell(smell: SmellArray, lifespan: Long, signalIndex: Int) extends SmellingCell {
  override type Self = StudentCell

  override def withSmell(smell: SmellArray): StudentCell = copy(smell = smell)
}

trait AlgaeAccessible[+T <: GridPart] {
  def withAlgae(lifespan: Long): T
}

object AlgaeAccessible {

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): AlgaeAccessible[StudentCell] =
    new AlgaeAccessible[StudentCell] {
      override def withAlgae(lifespan: Long): StudentCell = StudentCell(arg.smellWith(config.studentInitialSignal.toSignalVector), lifespan, config.studentSignalIndex)
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): AlgaeAccessible[BufferCell] =
    new AlgaeAccessible[BufferCell] {
      override def withAlgae(lifespan: Long): BufferCell = BufferCell(StudentCell(arg.smellWith(config.studentInitialSignal.toSignalVector), lifespan, config.studentSignalIndex))
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[AlgaeAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
