package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig


trait WorldType {
  def directions: Seq[Direction]
}

trait WorldShard {
  def cells: Map[CellId, Cell]

  def localCellIds: Set[CellId]

  def cellNeighbours: Map[CellId, Map[Direction, CellId]]

  def workerId: WorkerId

  def outgoingCells: Map[WorkerId, Set[CellId]]

  def incomingCells: Map[WorkerId, Set[CellId]]

  def outgoingWorkerNeighbours: Set[WorkerId] = outgoingCells.keySet + workerId

  def incomingWorkerNeighbours: Set[WorkerId] = incomingCells.keySet + workerId

  def cellToWorker: Map[CellId, WorkerId]

  def calculateSignalUpdates(iteration: Long, signalPropagation: SignalPropagation)(implicit config: XinukConfig): Map[CellId, SignalMap] = {
    cells.keys.map { cellId =>
      val neighbourStates = cellNeighbours(cellId)
        .filter { case (_, id) => localCellIds.contains(id) }
        .map { case (direction, neighbourId) => (direction, cells(neighbourId).state) }
      (cellId, signalPropagation.calculateUpdate(iteration, neighbourStates))
    }
  }.toMap
}

trait WorldBuilder {
  def apply(cellId: CellId): Cell

  def update(cellId: CellId, cellState: CellState): Unit

  def connectOneWay(from: CellId, direction: Direction, to: CellId): Unit

  final def connectTwoWay(from: CellId, direction: Direction, to: CellId): Unit = {
    connectOneWay(from, direction, to)
    connectOneWay(to, direction.opposite, from)
  }

  def build(): Map[WorkerId, WorldShard]
}
