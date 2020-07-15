package pl.edu.agh.xinuk.model

trait World {
  def cells: Map[CellId, Cell]

  def localCells: Map[CellId, Cell]

  def cellNeighbours: Map[CellId, Map[Direction, CellId]]

  def workerId: WorkerId

  def outgoingWorkerNeighbours: Set[WorkerId]

  def incomingWorkerNeighbours: Set[WorkerId]

  def cellToWorker: Map[CellId, WorkerId]

  def calculateSignalUpdates(signalPropagation: SignalPropagation)(implicit directions: Seq[Direction]): Map[CellId, SignalMap] = {
    cells.keys.map { cellId =>
      val neighbourSignals = cellNeighbours(cellId)
        .map { case (direction, neighbourId) => (direction, cells(neighbourId).state.signalMap) }
      (cellId, signalPropagation.calculateUpdate(directions, neighbourSignals))
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

  def build(): Map[WorkerId, World]
}
