package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}

object InitSmellPropagation {

  def calculateSmellAddends(cells: CellArray, x: Int, y: Int): Vector[Option[Signal]] = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
        )
      case (i, j) =>
        destinationCellSignal(i, j).map(signal => {
          signal(i)(j) + signal(i / 2 - j / 2 + 1)(j / 2 + i / 2) + signal(i / 2 + j / 2)(j /2 - i / 2 + 1)
        })
    }
  }

  def outOfBounds(a: Int, b: Int, cells: CellArray): Boolean = {
    a >= cells.length || b >= cells(0).length || a < 0 || b < 0
  }

}
