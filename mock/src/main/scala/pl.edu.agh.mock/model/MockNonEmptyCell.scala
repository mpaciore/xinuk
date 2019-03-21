package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class MockNonEmptyCell(smell: SmellArray) extends SmellingCell {
  override type Self = MockNonEmptyCell

  override def withSmell(smell: SmellArray): MockNonEmptyCell = copy(smell = smell)
}

object MockNonEmptyCell {
  def create(initialSignal: Signal): MockNonEmptyCell = MockNonEmptyCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}