package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}

object DefaultSmellPropagation {

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
          var signalSum = signal(i)(j)
          //          System.out.println(x + (j - i) / 2, y + (i + j - 2) / 2, x, y, i, j)
          if(!outOfBounds(x + (j - i) / 2, y + (i + j - 2) / 2, cells) && cells(x + (j - i) / 2)(y + (i + j - 2) / 2) == Obstacle)
            signalSum += signal(i / 2 + j / 2)(j /2 - i / 2 + 1) + signal(j)(2 - i)
          if(!outOfBounds(x + (i + j - 2) / 2, y + (i - j) / 2, cells) && cells(x + (i + j - 2) / 2)(y + (i - j) / 2) == Obstacle)
            signalSum += signal(i / 2 - j / 2 + 1)(j / 2 + i / 2) + signal(2 - j)(i)
          signalSum
        })
    }
  }

  def outOfBounds(a: Int, b: Int, cells: CellArray): Boolean = {
    a >= cells.length || b >= cells(0).length || a < 0 || b < 0
  }

}
