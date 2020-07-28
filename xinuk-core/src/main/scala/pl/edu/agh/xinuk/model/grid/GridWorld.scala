package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridDirection._

object GridWorldType extends WorldType {
  override def directions: Seq[Direction] = GridDirection.values
}

final class GridWorld(val cells: Map[CellId, Cell],
                      val cellNeighbours: Map[CellId, Map[Direction, CellId]],
                      val workerId: WorkerId,
                      val outgoingWorkerNeighbours: Set[WorkerId],
                      val incomingWorkerNeighbours: Set[WorkerId],
                      val cellToWorker: Map[CellId, WorkerId],
                     )(implicit config: XinukConfig) extends World {

  override def localCells: Map[CellId, Cell] = cells.filter { case (k, _) => cellToWorker(k) == workerId }

  def span: ((Int, Int), (Int, Int)) = {
    val coords = localCells.keys.map { case GridCellId(x, y) => (x, y) }
    val xMin = coords.map(_._1).min
    val xMax = coords.map(_._1).max
    val xSize = xMax - xMin + 1
    val yMin = coords.map(_._2).min
    val yMax = coords.map(_._2).max
    val ySize = yMax - yMin + 1
    ((xMin, yMin), (xSize, ySize))
  }
}

object GridWorld {
  def apply(cells: Map[CellId, Cell],
            cellNeighbours: Map[CellId, Map[Direction, CellId]],
            workerId: WorkerId,
            outgoingWorkerNeighbours: Set[WorkerId],
            incomingWorkerNeighbours: Set[WorkerId],
            cellToWorker: Map[CellId, WorkerId])(implicit config: XinukConfig): GridWorld =
    new GridWorld(cells, cellNeighbours, workerId, outgoingWorkerNeighbours, incomingWorkerNeighbours, cellToWorker)(config)
}

case class GridWorldBuilder()(implicit config: XinukConfig) extends WorldBuilder {

  import scala.collection.mutable.{Map => MutableMap}

  private val cellsMutable: MutableMap[CellId, Cell] = MutableMap.empty.withDefault(id => Cell.empty(id))
  private val neighboursMutable: MutableMap[CellId, MutableMap[Direction, CellId]] = MutableMap.empty.withDefault(_ => MutableMap.empty)

  override def apply(cellId: CellId): Cell = cellsMutable(cellId)

  override def update(cellId: CellId, cellState: CellState): Unit = cellsMutable(cellId) = Cell(cellId, cellState)

  def withWrappedBoundaries(): GridWorldBuilder = {
    def wrapped(cellId: GridCellId) = GridCellId(Math.floorMod(cellId.x, xSize), Math.floorMod(cellId.y, ySize))

    val boundary: Set[GridCellId] = Seq(
      (0 until xSize).map(x => GridCellId(x, 0)),
      (0 until xSize).map(x => GridCellId(x, ySize - 1)),
      (0 until ySize).map(y => GridCellId(0, y)),
      (0 until ySize).map(y => GridCellId(xSize - 1, y)),
    ).flatten.toSet

    for {
      from <- boundary
      direction <- GridDirection.values
      to = direction.of(from)
      if !valid(to)
    } connectOneWay(from, direction, wrapped(to))

    this
  }

  private def valid(cellId: GridCellId): Boolean = cellId.x >= 0 && cellId.x < xSize && cellId.y >= 0 && cellId.y < ySize

  private def ySize: Int = config.worldSize

  private def xSize: Int = config.worldSize

  override def connectOneWay(from: CellId, direction: Direction, to: CellId): Unit = {
    val cellNeighbours = neighboursMutable(from)
    cellNeighbours(direction) = to
    neighboursMutable(from) = cellNeighbours
  }

  def withGridConnections(): GridWorldBuilder = {
    for {
      x <- 0 until xSize
      y <- 0 until ySize
      direction <- GridDirection.values
      from = GridCellId(x, y)
      to = direction.of(from)
      if valid(to)
    } connectOneWay(from, direction, to)

    this
  }

  def build(): Map[WorkerId, GridWorld] = {
    val workerDomains = divide()

    val globalCellToWorker: Map[CellId, WorkerId] = workerDomains.flatMap {
      case (workerId, (localIds, _)) => localIds.map { cellId => (cellId, workerId) }
    }

    val outgoingNeighbours: Map[WorkerId, Set[WorkerId]] = workerDomains.map {
      case (workerId, (_, remoteIds)) => (workerId, remoteIds.map(globalCellToWorker) + workerId)
    }

    val incomingNeighbours: Map[WorkerId, Set[WorkerId]] = workerDomains.keys.map {
      id => (id, outgoingNeighbours
        .filter { case (_, outgoing) => outgoing.contains(id)}
        .map( { case (otherId, _) => otherId}).toSet)
    }.toMap

    workerDomains.map({ case (workerId, (localIds, remoteIds)) =>

      val cells = (localIds ++ remoteIds).map { id => (id, cellsMutable(id)) }.toMap

      val neighboursOfLocal = neighboursMutable
        .filter { case (id, _) => localIds.contains(id) }
        .map { case (id, cellNeighbours) => (id, cellNeighbours.toMap) }
        .toMap

      val neighboursOfRemote = neighboursMutable
        .filter { case (id, _) => remoteIds.contains(id) }
        .map { case (id, cellNeighbours) => (id, cellNeighbours.filter { case(_, nId) => localIds.contains(nId) }.toMap) }
        .toMap

      val neighbours = neighboursOfLocal ++ neighboursOfRemote

      val outgoingWorkers = outgoingNeighbours(workerId)

      val incomingWorkers = incomingNeighbours(workerId)

      val cellToWorker = globalCellToWorker.filter({ case (id, _) => localIds.contains(id) || remoteIds.contains(id) })

      (workerId, GridWorld(cells, neighbours, workerId, outgoingWorkers, incomingWorkers, cellToWorker))
    })
  }

  private def divide(): Map[WorkerId, (Set[CellId], Set[CellId])] = {
    val xWorkerCount = config.workersRoot
    val yWorkerCount = config.workersRoot

    val xSizes = split(xSize, xWorkerCount)
    val ySizes = split(ySize, yWorkerCount)

    val xOffsets = xSizes.scan(0) { case (acc, value) => acc + value }
    val yOffsets = ySizes.scan(0) { case (acc, value) => acc + value }

    val workerIds: Seq[WorkerId] = (1 to (xWorkerCount * yWorkerCount)).map(WorkerId)

    val workerSpans: Map[WorkerId, ((Int, Int), (Int, Int))] = workerIds.map { workerId =>
      val xPos = (workerId.value - 1) / yWorkerCount
      val yPos = (workerId.value - 1) % yWorkerCount
      val xOffset = xOffsets(xPos)
      val xSize = xSizes(xPos)
      val yOffset = yOffsets(yPos)
      val ySize = ySizes(yPos)
      (workerId, ((xOffset, xSize), (yOffset, ySize)))
    }.toMap

    workerSpans.map {
      case (workerId, ((xOffset, xSize), (yOffset, ySize))) =>

        val localIds: Set[CellId] = (for {
          x <- xOffset until (xOffset + xSize)
          y <- yOffset until (yOffset + ySize)
        } yield GridCellId(x, y)).toSet

        val remoteIds: Set[CellId] = localIds.flatMap(id => neighboursMutable(id).values).diff(localIds)

        (workerId, (localIds, remoteIds))
    }
  }

  private def split(value: Int, parts: Int): Seq[Int] = {
    if (parts <= 0) {
      Seq.empty
    } else {
      val quotient: Int = value / parts
      val remainder: Int = value % parts

      Seq.tabulate(parts) {
        case index if index < remainder => quotient + 1
        case _ => quotient
      }
    }
  }
}