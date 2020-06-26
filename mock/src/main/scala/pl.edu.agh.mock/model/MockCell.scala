package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.{Cell, Signal}
import pl.edu.agh.xinuk.model.Cell.SmellMap


final case class MockCell(smell: SmellMap) extends Cell {
  override type Self = MockCell

  override def withSmell(smell: SmellMap): MockCell = copy(smell = smell)
}

object MockCell {
  def apply(initialSignal: Signal): MockCell = MockCell(Cell.uniformSignal(initialSignal))
}