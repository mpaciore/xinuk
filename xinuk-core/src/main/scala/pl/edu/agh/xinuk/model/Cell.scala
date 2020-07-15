package pl.edu.agh.xinuk.model

trait CellId

trait CellContents

case object Empty extends CellContents

case object Obstacle extends CellContents

case class CellState(contents: CellContents, signalMap: SignalMap) {
  def withSignal(signalMap: SignalMap): CellState = CellState(contents, signalMap)
}

case class Cell(id: CellId, var state: CellState) {
  def update(newState: CellState): Unit = {
    state = newState
  }

  def updateSignal(signalUpdate: SignalMap): Unit = {
    state = CellState(state.contents, signalUpdate)
  }
}

object Cell {
  def empty(id: CellId)(implicit directions: Seq[Direction]): Cell =
    create(id, CellState(Empty, SignalMap.empty))

  def obstacle(id: CellId)(implicit directions: Seq[Direction]): Cell =
    create(id, CellState(Obstacle, SignalMap.empty))

  def create(id: CellId, state: CellState): Cell =
    Cell(id, state)
}
