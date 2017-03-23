package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig

import scala.collection.immutable.Seq

final case class Grid(cells: Array[Array[Cell]]) {

  import Grid._

  private val cellSignalFun: (Int) => (Int) => Option[Array[Array[Signal]]] = {
    cells.map(_.lift).lift.andThen {
      case Some(rowFunction) => rowFunction.andThen(_.map(_.smell))
      case None => _: Int => None
    }
  }

  def propagatedSignal(x: Int, y: Int)(implicit config: ForminConfig): Array[Array[Signal]] = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[Array[Array[Signal]]] = {
      cellSignalFun(x + i - 1)(y + j - 1)
    }

    val current = cells(x)(y).smell
    val addends = SubcellCoordinates.map {
      case (i, j) if i == 1 || j == 1 =>
        destinationCellSignal(i, j).map(signal =>
          signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
        )
      case (i, j) => destinationCellSignal(i, j).map(_.apply(i)(j))
    }
    addends.foldLeft((Cell.emptySignal, 0)) { case ((cell, index), signalOpt) =>
      val (i, j) = SubcellCoordinates(index)
      cell(i)(j) = current(i)(j) + signalOpt.getOrElse(Signal.Zero)
      (cell, index + 1)
    }._1
  }
}

object Grid {
  def empty(n: Int): Grid = {
    require(n > 3, "Insufficient grid size, no cells would be empty.")
    val values = Array.tabulate[Cell](n, n) {
      case (x, y) if x == 0 || x == n - 1 || y == 0 || y == n - 1 => Obstacle
      case _ => EmptyCell()
    }
    Grid(values)
  }

  val SubcellCoordinates: Vector[(Int, Int)] = {
    val pos = Vector(0, 1, 2)
    pos.flatMap(i => pos.map(j => (i, j))).filter { case (i, j) => !(i == 1 && j == 1) }
  }

  val SubcellCoordinatesWithIndices: Seq[((Int, Int), Int)] = SubcellCoordinates.zipWithIndex
}

final case class Signal(value: Double) extends AnyVal {
  def +(other: Signal) = Signal(value + other.value)
}

object Signal {
  val Zero = Signal(0d)
}

final case class Energy(value: Double) extends AnyVal

sealed trait Cell extends Any {
  def smell: Array[Array[Signal]]
}

object Cell {
  final val Size = 3

  def emptySignal: Array[Array[Signal]] = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

sealed trait HasEnergy {
  self: Cell =>
  def energy: Energy
}

final case class ForaminiferaCell(energy: Energy, smell: Array[Array[Signal]]) extends Cell with HasEnergy

final case class AlgaeCell(energy: Energy, smell: Array[Array[Signal]]) extends Cell with HasEnergy

case object Obstacle extends Cell {
  override def smell: Array[Array[Signal]] = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

final case class EmptyCell(smell: Array[Array[Signal]] = Cell.emptySignal) extends AnyVal with Cell