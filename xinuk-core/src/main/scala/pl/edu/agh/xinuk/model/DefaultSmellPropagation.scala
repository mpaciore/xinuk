package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}

object DefaultSmellPropagation {

  def calculateSmellAddendsStandard(cells: CellArray, x: Int, y: Int): Vector[Option[Signal]] = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
        )
      case (i, j) =>
        destinationCellSignal(i, j).map(_.apply(i)(j))
    }
  }

  def calculateSmellAddendsCircular(cells: CellArray, x: Int, y: Int): Vector[Option[Signal]] = {
    def sideToSide = 1.0 / 3
    def sideToCorner = 1.0 / Math.sqrt(10)
    def cornerToSide = 1.0 / Math.sqrt(13)
    def cornerToCorner = 1.0 / (3 * Math.sqrt(2))

    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) * sideToSide + (signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)) * cornerToSide
        )
      case (i, j) =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) * cornerToCorner + (signal(i)(1) + signal(1)(j)) * sideToCorner
        )
    }
  }
}
