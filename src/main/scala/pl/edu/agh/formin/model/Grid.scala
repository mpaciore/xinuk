package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig

final case class Grid(cells: Array[Array[Cell]]) {

  import Grid._

  private val cellSignalFun: (Int) => (Int) => Option[Array[Array[Signal]]] = {
    cells.map(_.lift).lift.andThen {
      case Some(rowFunction) => rowFunction.andThen(_.map(_.smell))
      case None => _: Int => None
    }
  }

  def propagatedSignal(x: Int, y: Int)(implicit config: ForminConfig): Cell = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[Array[Array[Signal]]] = {
      cellSignalFun(x + i - 1)(y + j - 1)
    }

    val current = cells(x)(y)
    current match {
      case Obstacle => current
      case smelling: SmellMedium[_] =>
        val currentSmell = current.smell
        val addends = SubcellCoordinates.map {
          case (i, j) if i == 1 || j == 1 =>
            destinationCellSignal(i, j).map(signal =>
              signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
            )
          case (i, j) =>
            destinationCellSignal(i, j).map(_.apply(i)(j))
        }
        val (newSmell, _) = addends.foldLeft((Cell.emptySignal, 0)) { case ((cell, index), signalOpt) =>
          val (i, j) = SubcellCoordinates(index)
          cell(i)(j) = currentSmell(i)(j) + signalOpt.getOrElse(Signal.Zero) * config.signalSuppresionFactor
          (cell, index + 1)
        }
        smelling.withSmell(newSmell)
    }
  }
}

object Grid {
  def empty(implicit config: ForminConfig): Grid = {
    val n = config.gridSize
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

}

final case class Signal(value: Double) extends AnyVal {
  def +(other: Signal) = Signal(value + other.value)

  def *(factor: Double) = Signal(value * factor)
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

sealed trait SmellMedium[T <: Cell with SmellMedium[T]] {
  self: T =>
  type Self >: self.type <: T

  def withSmell(smell: Array[Array[Signal]]): Self
}

sealed trait HasEnergy {
  self: Cell =>
  def energy: Energy
}

final case class ForaminiferaCell(energy: Energy, smell: Array[Array[Signal]]) extends Cell with HasEnergy with SmellMedium[ForaminiferaCell] {
  type Self = ForaminiferaCell

  override def withSmell(smell: Array[Array[Signal]]) = copy(smell = smell)
}

final case class AlgaeCell(smell: Array[Array[Signal]]) extends Cell with SmellMedium[AlgaeCell] {
  type Self = AlgaeCell

  override def withSmell(smell: Array[Array[Signal]]) = copy(smell = smell)
}

case object Obstacle extends Cell {
  override def smell: Array[Array[Signal]] = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

final case class EmptyCell(smell: Array[Array[Signal]] = Cell.emptySignal) extends Cell with SmellMedium[EmptyCell] {
  type Self = EmptyCell

  override def withSmell(smell: Array[Array[Signal]]) = copy(smell = smell)

  private def smellWithSignal(added: Signal): Array[Array[Signal]] = {
    Array.tabulate(Cell.Size, Cell.Size)((i, j) => smell(i)(j) + added)
  }

  def withForaminifera(energy: Energy)(implicit config: ForminConfig): ForaminiferaCell = {
    ForaminiferaCell(energy, smellWithSignal(config.foraminiferaInitialSignal))
  }

  def withAlgae(implicit config: ForminConfig): AlgaeCell = {
    AlgaeCell(smellWithSignal(config.algaeInitialSignal))
  }

}