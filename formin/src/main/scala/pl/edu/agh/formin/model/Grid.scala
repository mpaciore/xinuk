package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

object ForaminiferaAccessible {
  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[(Energy, Long) => GridPart] = arg match {
    case cell@AlgaeCell(_, _) => Some((energy, lifespan) =>
      ForaminiferaCell(energy + config.algaeEnergeticCapacity, cell.smellWith(config.foraminiferaInitialSignal), lifespan)
    )
    case cell@EmptyCell(_) => Some((energy, lifespan) =>
      ForaminiferaCell(energy, cell.smellWith(config.foraminiferaInitialSignal), lifespan)
    )
    case cell@BufferCell(_) => Some((energy, lifespan) =>
      BufferCell(ForaminiferaCell(energy, cell.smellWith(config.foraminiferaInitialSignal), lifespan))
    )
    case _ => None
  }
}

sealed trait AlgaeAccessible extends GridPart {
  def withAlgae(lifespan : Long)(implicit config: ForminConfig): GridPart
}

final case class ForaminiferaCell(energy: Energy, smell: SmellArray, lifespan : Long) extends SmellingCell {
  override type Self = ForaminiferaCell

  override def withSmell(smell: SmellArray): ForaminiferaCell = copy(smell = smell)
}

final case class AlgaeCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)
}

final case class BufferCell(cell: SmellingCell)
  extends SmellMedium with GridPart with AlgaeAccessible {

  override type Self = BufferCell

  override def smell: SmellArray = cell.smell

  override def withSmell(smell: SmellArray): BufferCell = {
    BufferCell(cell.withSmell(smell))
  }

  override def withAlgae(lifespan : Long)(implicit config: ForminConfig): BufferCell = {
    BufferCell(AlgaeCell(smellWith(config.algaeInitialSignal), lifespan))
  }

}

final case class EmptyCell(smell: SmellArray) extends SmellingCell with AlgaeAccessible {
  override type Self = EmptyCell

  override def withSmell(smell: SmellArray): EmptyCell = copy(smell)

  override def withAlgae(lifespan : Long)(implicit config: ForminConfig): AlgaeCell = {
    AlgaeCell(smellWith(config.algaeInitialSignal), lifespan)
  }

}

object EmptyCell {
  final val Instance: EmptyCell = EmptyCell(Cell.emptySignal)
}