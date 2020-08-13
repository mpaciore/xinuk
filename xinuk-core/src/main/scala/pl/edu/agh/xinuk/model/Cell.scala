package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig

trait CellId

trait CellContents {
  def passiveSignal()(implicit config: XinukConfig): SignalMap = SignalMap.empty
}

case object Empty extends CellContents

case object Obstacle extends CellContents

case class CellState(contents: CellContents, signalMap: SignalMap) {
  def withSignal(signalMap: SignalMap): CellState = CellState(contents, signalMap)
}

object CellState {
  def empty()(implicit config: XinukConfig): CellState =
    CellState(Empty)

  def obstacle()(implicit config: XinukConfig): CellState =
    CellState(Obstacle)

  def apply(contents: CellContents)(implicit config: XinukConfig): CellState =
    CellState(contents, SignalMap.empty)
}

case class Cell(id: CellId, var state: CellState) {
  def update(newState: CellState): Unit = {
    state = newState
  }

  def updateContents(contents: CellContents): Unit = {
    state = CellState(contents, state.signalMap)
  }

  def updateSignal(signalUpdate: SignalMap): Unit = {
    state = CellState(state.contents, signalUpdate)
  }
}

object Cell {
  def empty(id: CellId)(implicit config: XinukConfig): Cell =
    Cell(id, CellState.empty())

  def obstacle(id: CellId)(implicit config: XinukConfig): Cell =
    Cell(id, CellState.obstacle())
}
