package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class ForaminiferaCell(energy: Energy, smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = ForaminiferaCell

  override def withSmell(smell: SmellArray): ForaminiferaCell = copy(smell = smell)
}


sealed trait ForaminiferaAccessible {
  def withForaminifera(energy: Energy, lifespan: Long): GridPart
}

object ForaminiferaAccessible {
  private def accessibleFactory(f: (Energy, Long) => GridPart): Some[ForaminiferaAccessible] = Some(new ForaminiferaAccessible {
    override def withForaminifera(energy: Energy, lifespan: Long): GridPart = f(energy, lifespan)
  })

  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[ForaminiferaAccessible] = arg match {
    case cell@AlgaeCell(_, _) => accessibleFactory((energy, lifespan) =>
      ForaminiferaCell(energy + config.algaeEnergeticCapacity, cell.smellWith(config.foraminiferaInitialSignal), lifespan)
    )
    case cell@EmptyCell(_) => accessibleFactory((energy, lifespan) =>
      ForaminiferaCell(energy, cell.smellWith(config.foraminiferaInitialSignal), lifespan)
    )
    case cell@BufferCell(_) => accessibleFactory((energy, lifespan) =>
      BufferCell(ForaminiferaCell(energy, cell.smellWith(config.foraminiferaInitialSignal), lifespan))
    )
    case _ => None
  }
}
