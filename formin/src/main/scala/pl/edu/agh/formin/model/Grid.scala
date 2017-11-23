package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._


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

final case class ForaminiferaCell(energy: Energy, smell: SmellArray, lifespan : Long) extends SmellingCell {
  override type Self = ForaminiferaCell

  override def withSmell(smell: SmellArray): ForaminiferaCell = copy(smell = smell)
}

final case class AlgaeCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)
}