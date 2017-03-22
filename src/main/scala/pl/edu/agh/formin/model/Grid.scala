package pl.edu.agh.formin.model

final case class Grid(cells: Array[Array[Cell]]) {
  private val cellSignalFun: (Int) => (Int) => Option[Array[Array[Signal]]] = {
    cells.map(_.lift).lift.andThen {
      case Some(rowFunction) => rowFunction.andThen(_.map(_.smell))
      case None => _: Int => None
    }
  }

  def propagatedSignal(x: Int, y: Int): Array[Array[Signal]] = ???
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
}

final case class Signal(value: Double) extends AnyVal

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