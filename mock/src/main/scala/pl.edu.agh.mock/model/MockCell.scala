package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Grid, Signal, SmellingCell}

final case class MockCell(smell: SmellArray, crowd: Int, destinationPoint: (Int, Int)) extends SmellingCell {

  override type Self = MockCell

  override def withSmell(smell: SmellArray): MockCell = copy(smell = smell)
}

object MockCell {
  def create(initialSignal: Signal, initialCrowd: Int = 1, grid: Grid): MockCell =
    MockCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), initialCrowd, POIFactory.generatePOI(grid)())
}

