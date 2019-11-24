package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.{CellArray, SubcellCoordinates}

object DefaultSmellPropagation {

  def calculateSmellAddendsStandard(config: XinukConfig): (CellArray, Int, Int) => Vector[Option[Signal]] = {
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
    calculateSmellAddendsStandard
  }

  def calculateSmellAddendsCircular(config: XinukConfig): (CellArray, Int, Int) => Vector[Option[Signal]] = {
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
    calculateSmellAddendsCircular
  }

  def calculateSmellAddendsCurved(config: XinukConfig): (CellArray, Int, Int) => Vector[Option[Signal]] = {
    def calculateSmellAddendsCurved(cells: CellArray, x: Int, y: Int): Vector[Option[Signal]] = {
      @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
        cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
      }

      val crossWeight: Double = config.crossBendFactor
      val straightWeight: Double = config.straightBendFactor

      SubcellCoordinates.map {
        case (i, j) if i == 1 || j == 1 =>
          destinationCellSignal(i, j).map(signal =>
            signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
          )
        case (i, j) if i == 0 && j == 0 =>
          destinationCellSignal(i, j).map(signal => {
            var signalSum = signal(0)(0)
            if (cells(x - 1)(y) == Obstacle)
              signalSum += (signal(0)(1) * straightWeight + signal(0)(2) * crossWeight)
            if (cells(x)(y - 1) == Obstacle)
              signalSum += (signal(1)(0) * straightWeight + signal(2)(0) * crossWeight)
            signalSum
          })
        case (i, j) if i == 2 && j == 0 =>
          destinationCellSignal(i, j).map(signal => {
            var signalSum = signal(2)(0)
            if (cells(x + 1)(y) == Obstacle)
              signalSum += (signal(2)(1) * straightWeight + signal(2)(2) * crossWeight)
            if (cells(x)(y - 1) == Obstacle)
              signalSum += (signal(0)(0) * straightWeight + signal(1)(0) * crossWeight)
            signalSum
          })
        case (i, j) if i == 0 && j == 2 =>
          destinationCellSignal(i, j).map(signal => {
            var signalSum = signal(0)(2)
            if (cells(x - 1)(y) == Obstacle)
              signalSum += (signal(0)(0) * straightWeight + signal(0)(1) * crossWeight)
            if (cells(x)(y + 1) == Obstacle)
              signalSum += (signal(1)(2) * straightWeight + signal(2)(2) * crossWeight)
            signalSum
          })
        case (i, j) if i == 2 && j == 2 =>
          destinationCellSignal(i, j).map(signal => {
            var signalSum = signal(2)(2)
            if (cells(x + 1)(y) == Obstacle)
              signalSum += (signal(2)(0) * straightWeight + signal(2)(1) * crossWeight)
            if (cells(x)(y + 1) == Obstacle)
              signalSum += (signal(0)(2) * straightWeight + signal(1)(2) * crossWeight)
            signalSum
          })
      }
    }
    calculateSmellAddendsCurved
  }
}
