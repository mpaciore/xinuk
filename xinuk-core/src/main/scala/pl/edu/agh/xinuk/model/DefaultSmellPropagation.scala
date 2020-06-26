package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.EnhancedCell.NeighbourMap

object DefaultSmellPropagation {

  @inline def destinationCellSignal(grid: EnhancedGrid,
                                    neighbours: NeighbourMap,
                                    direction: Direction): Option[SmellMap] = {
    neighbours.get(direction)
      .flatMap({ case (x, y) => grid.getLocalCellOptionAt(x, y) })
      .map(_.cell.smell)
  }

  def calculateSmellAddendsStandard(grid: EnhancedGrid, neighbours: NeighbourMap): SmellMap =
    Direction.values.map({
      case cardinal@(Direction.Top | Direction.Right | Direction.Bottom | Direction.Left) =>
        (
          cardinal,
          destinationCellSignal(grid, neighbours, cardinal)
            .map(signal => cardinal.withAdjacent.map(signal(_)).reduce(_ + _))
            .getOrElse(Signal.Zero)
        )
      case diagonal@(Direction.TopLeft | Direction.TopRight | Direction.BottomRight | Direction.BottomLeft) =>
        (
          diagonal,
          destinationCellSignal(grid, neighbours, diagonal)
            .map(_ (diagonal))
            .getOrElse(Signal.Zero)
        )
    }).toMap

  def calculateSmellAddendsCircular(grid: EnhancedGrid, neighbours: NeighbourMap): SmellMap = {
    def sideToSide: Double = 1.0 / 3

    def sideToCorner: Double = 1.0 / Math.sqrt(10)

    def cornerToSide: Double = 1.0 / Math.sqrt(13)

    def cornerToCorner: Double = 1.0 / (3 * Math.sqrt(2))

    Direction.values.map({
      case cardinal@(Direction.Top | Direction.Right | Direction.Bottom | Direction.Left) =>
        (
          cardinal,
          destinationCellSignal(grid, neighbours, cardinal)
            .map(signal => signal(cardinal) * sideToSide + cardinal.adjacent.map(signal(_)).reduce(_ + _) * cornerToSide)
            .getOrElse(Signal.Zero)
        )
      case diagonal@(Direction.TopLeft | Direction.TopRight | Direction.BottomRight | Direction.BottomLeft) =>
        (
          diagonal,
          destinationCellSignal(grid, neighbours, diagonal)
            .map(signal => signal(diagonal) * cornerToCorner + diagonal.adjacent.map(signal(_)).reduce(_ + _) * sideToCorner)
            .getOrElse(Signal.Zero)
        )
    }).toMap
  }
}
