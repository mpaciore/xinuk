package pl.edu.agh.school.model

import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.SignalVector.SignalVectorOps

final case class WallCell(smell: SmellArray) extends SmellingCell {
  override type Self = WallCell

  override def withSmell(smell: SmellArray): WallCell = copy(smell = smell)
}

trait WallAccessible[+T <: GridPart] {
  def withWall(energy: Energy, lifespan: Long): T
}

object WallAccessible {

  def unapply(arg: StudentCell)(implicit config: SchoolConfig): WallAccessible[WallCell] =
    new WallAccessible[WallCell] {
      override def withWall(energy: Energy, lifespan: Long): WallCell = WallCell(arg.smellWith(config.cleanerInitialSignal.toSignalVector))
    }

  def unapply(arg: EmptyCell)(implicit config: SchoolConfig): WallAccessible[WallCell] =
    new WallAccessible[WallCell] {
      override def withWall(energy: Energy, lifespan: Long): WallCell = WallCell(arg.smellWith(config.cleanerInitialSignal.toSignalVector))
    }

  def unapply(arg: BufferCell)(implicit config: SchoolConfig): WallAccessible[BufferCell] =
    new WallAccessible[BufferCell] {
      override def withWall(energy: Energy, lifespan: Long): BufferCell = BufferCell(WallCell(arg.smellWith(config.cleanerInitialSignal.toSignalVector)))
    }

  def unapply(arg: GridPart)(implicit config: SchoolConfig): Option[WallAccessible[GridPart]] = arg match {
    case cell: StudentCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
