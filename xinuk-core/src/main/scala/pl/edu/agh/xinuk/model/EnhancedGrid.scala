package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.{SmellMap, SmellMapOps}
import pl.edu.agh.xinuk.model.Direction.Direction

class EnhancedGrid(private val cells: Array[Array[LocalEnhancedCell]],
                   private val remoteCells: collection.mutable.Map[(Int, Int), RemoteEnhancedCell],
                   private val offset: (Int, Int))
                  (implicit config: XinukConfig) {
  def xSize: Int = cells.length

  def ySize: Int = cells(0).length

  def xRange: Seq[Int] = (0 until xSize).map(_ + offset._1)

  def yRange: Seq[Int] = (0 until ySize).map(_ + offset._2)

  def indicesRowWise: Seq[(Int, Int)] = for {x <- xRange; y <- yRange} yield (x, y)

  def indicesColumnWise: Seq[(Int, Int)] = for {y <- yRange; x <- xRange} yield (x, y)

  def getCellAt(x: Int, y: Int): EnhancedCell = {
    if (areCoordsInGrid(x, y)) {
      getLocalCellAt(x, y)
    } else {
      remoteCells((x, y))
    }
  }

  def getLocalCellAt(x: Int, y: Int): LocalEnhancedCell = {
    val (localX, localY) = toLocalCoords(x, y)
    cells(localX)(localY)
  }

  def setCellAt(x: Int, y: Int, newCell: GridPart): Unit = {
    getCellAt(x, y) match {
      case localCell: LocalEnhancedCell =>
        val (localX, localY) = toLocalCoords(x, y)
        cells(localX)(localY) = localCell.withCell(newCell)
      case remoteCell: RemoteEnhancedCell =>
        remoteCells((x, y)) = remoteCell.withCell(newCell)
    }
  }

  def getLocalCellOptionAt(x: Int, y: Int): Option[LocalEnhancedCell] = Option((x, y))
    .filter({ case (x, y) => areCoordsInGrid(x, y) })
    .map({ case (x, y) => toLocalCoords(x, y) })
    .map({ case (localX, localY) => cells(localX)(localY) })

  def divide(workersRoot: Int, workerIds: Seq[WorkerId]): Seq[(WorkerId, EnhancedGrid, Set[WorkerId])] = {
    val xOffsets: Seq[Int] = Range.BigDecimal(0.0, xSize.toDouble, xSize.toDouble / workersRoot.toDouble)
      .take(workersRoot)
      .map(bd => Math.round(bd.floatValue()))
    val yOffsets: Seq[Int] = Range.BigDecimal(0.0, ySize.toDouble, ySize.toDouble / workersRoot.toDouble)
      .take(workersRoot)
      .map(bd => Math.round(bd.floatValue()))

    val xOffsetsWithSizes: Seq[(Int, Int)] = xOffsets.zip(xOffsets.takeRight(workersRoot - 1) :+ xSize)
      .map({ case (offset, nextOffset) => (offset, nextOffset - offset) })
    val yOffsetsWithSizes: Seq[(Int, Int)] = yOffsets.zip(yOffsets.takeRight(workersRoot - 1) :+ ySize)
      .map({ case (offset, nextOffset) => (offset, nextOffset - offset) })

    val subGridRectangles: Seq[(Int, Int, Int, Int)] = for {
      (xOffset, xSize) <- xOffsetsWithSizes
      (yOffset, ySize) <- yOffsetsWithSizes
    } yield (xOffset, yOffset, xSize, ySize)

    def coordinatesToWorkerId: (Int, Int) => WorkerId = (x, y) => {
      val xWorkerIndex: Int = xOffsets.zipWithIndex.takeWhile({ case (offset, _) => x >= offset }).last._2
      val yWorkerIndex: Int = yOffsets.zipWithIndex.takeWhile({ case (offset, _) => y >= offset }).last._2
      workerIds(xWorkerIndex * workersRoot + yWorkerIndex)
    }

    subGridRectangles.zip(workerIds).map({
      case ((xOffset, yOffset, xSize, ySize), workerId) =>
        val subGrid: Array[Array[LocalEnhancedCell]] = Array.tabulate[LocalEnhancedCell](xSize, ySize) { case (x, y) =>
          cells(x + xOffset)(y + yOffset)
        }
        val subRemoteCells: Map[(Int, Int), RemoteEnhancedCell] = subGrid.zipWithIndex.flatMap({
          case (row, xIndex) => row.zipWithIndex.flatMap({
            case (localCell, yIndex) => localCell.neighbours.map({
              case (direction, (toX, toY)) => ((xIndex + xOffset, yIndex + yOffset), direction, (toX, toY))
            })
          })
        }).groupBy({ case (_, _, to) => to })
          .filterNot({ case ((x, y), _) => coordinatesToWorkerId(x, y) == workerId })
          .map({ case ((x, y), connections) =>
            val neighbours = connections.map({ case (from, direction, _) => (direction.opposite, from) }).toMap
            ((x, y), RemoteEnhancedCell(neighbours, coordinatesToWorkerId(x, y), (x, y)))
          })
        val enhancedSubGrid: EnhancedGrid = EnhancedGrid(subGrid, collection.mutable.Map(subRemoteCells.toList: _*), (xOffset, yOffset))

        val neighbourWorkers: Set[WorkerId] = subRemoteCells.map(_._2.workerId).toSet.filterNot(_ == workerId)

        (workerId, enhancedSubGrid, neighbourWorkers)
    })
  }

  def propagatedSignal(smellPropagationFunction: (EnhancedGrid, Map[Direction, (Int, Int)]) => SmellMap): EnhancedGrid = {
    EnhancedGrid(
      tabulateLocalCells(propagatedLocal(smellPropagationFunction, _)),
      mapRemoteCells(propagatedRemote(smellPropagationFunction, _)),
      offset
    )
  }

  private def tabulateLocalCells(fun: LocalEnhancedCell => LocalEnhancedCell): Array[Array[LocalEnhancedCell]] =
    Array.tabulate[LocalEnhancedCell](xSize, ySize) { (x, y) => fun(cells(x)(y)) }

  private def mapRemoteCells(fun: RemoteEnhancedCell => RemoteEnhancedCell): collection.mutable.Map[(Int, Int), RemoteEnhancedCell] =
    remoteCells.map({ case ((x, y), currentRemoteCell) => ((x, y), fun(currentRemoteCell))})

  private def propagatedLocal(smellPropagationFunction: (EnhancedGrid, Map[Direction, (Int, Int)]) => SmellMap,
                               oldCell: LocalEnhancedCell): LocalEnhancedCell =
    oldCell.withCell(propagatedGridPart(smellPropagationFunction, oldCell))

  private def propagatedRemote(smellPropagationFunction: (EnhancedGrid, Map[Direction, (Int, Int)]) => SmellMap,
                              oldCell: RemoteEnhancedCell): RemoteEnhancedCell =
    oldCell.withCell(propagatedGridPart(smellPropagationFunction, oldCell))

  private def propagatedGridPart(smellPropagationFunction: (EnhancedGrid, Map[Direction, (Int, Int)]) => SmellMap,
                                 enhancedCell: EnhancedCell): GridPart =
    enhancedCell.cell match {
      case obstacle@Obstacle => obstacle
      case smelling: GridPart =>
        val currentSmell: SmellMap = smelling.smell
        val addends: SmellMap = smellPropagationFunction(this, enhancedCell.neighbours)
        val newSmell: SmellMap = currentSmell * config.signalAttenuationFactor + addends * config.signalSuppressionFactor
        smelling.withSmell(newSmell)
    }

  def clearOutgoingCells(): Unit = {
    remoteCells.foreach({ case ((x, y), cell) => remoteCells((x, y)) = cell.empty() })
  }

  def outgoingCells(targetWorker: WorkerId): Set[((Int, Int), GridPart)] = remoteCells.values
    .filter(_.workerId == targetWorker)
    .map(remoteCell => (remoteCell.targetCoordinates, remoteCell.cell))
    .toSet

  private def areCoordsInGrid(x: Int, y: Int): Boolean = {
    val (localX, localY) = toLocalCoords(x, y)
    0 <= localX && localX < xSize && 0 <= localY && localY < ySize
  }

  private def toLocalCoords(x: Int, y: Int): (Int, Int) = (x - offset._1, y - offset._2)

  def emptyCopy(emptyCellFactory: => GridPart = EmptyCell.Instance): EnhancedGrid = {
    val emptyGrid: Array[Array[LocalEnhancedCell]] = cells.map(_.map(_.withCell(emptyCellFactory)))
    EnhancedGrid(emptyGrid, remoteCells, offset)
  }
}

object EnhancedGrid {

  def empty(emptyCellFactory: => GridPart = EmptyCell.Instance)(implicit config: XinukConfig): EnhancedGrid = {
    EnhancedGrid(Grid(Array.tabulate[GridPart](config.gridSize, config.gridSize)((_, _) => emptyCellFactory)), NonPlanarConnections.empty)
  }

  def apply(grid: Grid, nonPlanarConnections: NonPlanarConnections)(implicit config: XinukConfig): EnhancedGrid = {
    val nonPlanarConnectionMap: Map[(Int, Int), Set[(Direction, (Int, Int))]] = nonPlanarConnections.toMap

    val enhancedGrid = Array.tabulate[LocalEnhancedCell](config.gridSize, config.gridSize) {
      (cellX, cellY) => {
        val planarNeighbours: Map[Direction, (Int, Int)] = Direction
          .values
          .map(d => (d, d.of(cellX, cellY)))
          .filter({ case (_, (x, y)) => areCoordinatesValid(x, y) })
          .toMap
        val nonPlanarNeighbours: Map[Direction, (Int, Int)] = nonPlanarConnectionMap
          .getOrElse((cellX, cellY), Set.empty)
          .toMap
        val neighbours: Map[Direction, (Int, Int)] = planarNeighbours ++ nonPlanarNeighbours
        LocalEnhancedCell(grid.cells(cellX)(cellY), neighbours)
      }
    }
    EnhancedGrid(enhancedGrid, collection.mutable.Map.empty, (0, 0))
  }

  def apply(grid: Array[Array[LocalEnhancedCell]],
            remoteCells: collection.mutable.Map[(Int, Int), RemoteEnhancedCell],
            offset: (Int, Int)
           )(implicit config: XinukConfig): EnhancedGrid =
    new EnhancedGrid(grid, remoteCells, offset)

  private def areCoordinatesValid(x: Int, y: Int)(implicit config: XinukConfig): Boolean = {
    0 <= x && x < config.gridSize && 0 <= y && y < config.gridSize
  }
}

final class NonPlanarConnections(private val connections: Set[NonPlanarConnection]) {
  def toMap: Map[(Int, Int), Set[(Direction, (Int, Int))]] = {
    connections.map { case NonPlanarConnection(from, direction, to) => (from, (direction, to)) }
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }
}

object NonPlanarConnections {
  private val EMPTY = NonPlanarConnections(Set.empty)

  def apply(nonPlanarConnections: Set[NonPlanarConnection]): NonPlanarConnections =
    new NonPlanarConnections(nonPlanarConnections)

  def empty: NonPlanarConnections = EMPTY
}

final case class NonPlanarConnection(from: (Int, Int), direction: Direction, to: (Int, Int)) {
  def reversed: NonPlanarConnection =
    NonPlanarConnection(to, direction.opposite, from)
}

object NonPlanarConnection {
  def apply(from: (Int, Int), direction: Direction, to: (Int, Int)): NonPlanarConnection =
    new NonPlanarConnection(from, direction, to)
}
