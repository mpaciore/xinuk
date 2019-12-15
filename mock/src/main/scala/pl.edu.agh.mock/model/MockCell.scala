package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class MockCell(smell: SmellMap) extends SmellingCell {
  override type Self = MockCell

  override def withSmell(smell: SmellMap): MockCell = copy(smell = smell)
}

object MockCell {
  def create(initialSignal: Signal): MockCell = MockCell(Cell.uniformSignal(initialSignal))
}