package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SignalVector, SmellingCell}

final case class MockCell(smell: SmellArray) extends SmellingCell {
  override type Self = MockCell

  override def withSmell(smell: SmellArray): MockCell = copy(smell = smell)
}

object MockCell {
  def create(initialSignal: SignalVector): MockCell = MockCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}