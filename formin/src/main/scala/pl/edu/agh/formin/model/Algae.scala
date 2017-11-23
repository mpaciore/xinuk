package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}


final case class AlgaeCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)
}

sealed trait AlgaeAccessible {
  def withAlgae(lifespan: Long): GridPart
}

object AlgaeAccessible {
  private def accessibleFactory(f: Long => GridPart): Some[AlgaeAccessible] = Some(new AlgaeAccessible {
    override def withAlgae(lifespan: Long): GridPart = f(lifespan)
  })

  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[AlgaeAccessible] = arg match {
    case cell@EmptyCell(_) => accessibleFactory(lifespan =>
      AlgaeCell(cell.smellWith(config.algaeInitialSignal), lifespan)
    )
    case cell@BufferCell(_) => accessibleFactory(lifespan =>
      BufferCell(AlgaeCell(cell.smellWith(config.algaeInitialSignal), lifespan))
    )
    case _ => None
  }
}
