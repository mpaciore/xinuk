package pl.edu.agh.mock.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class MockCell(smell: SmellArray, crowd: List[MockCell], var destinationPoint: Point, workerId: WorkerId) extends SmellingCell {

  override type Self = MockCell

  override def withSmell(smell: SmellArray): MockCell = copy(smell = smell)
}

object MockCell {
  def create(initialSignal: Signal, initialCrowd: List[MockCell] = List(), destinationPoint: Point,workerId: WorkerId): MockCell =
    MockCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), initialCrowd, destinationPoint, workerId)
}
