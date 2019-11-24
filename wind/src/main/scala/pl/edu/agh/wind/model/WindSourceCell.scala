package pl.edu.agh.wind.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class WindSourceCell(smell: SmellArray) extends SmellingCell {
  override type Self = WindSourceCell

  override def withSmell(smell: SmellArray): WindSourceCell = copy(smell = smell)
}

object WindSourceCell {
  def create(initialSignal: Signal): WindSourceCell = WindSourceCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}