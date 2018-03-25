package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.Grid.CellArray

final case class Grid(cells: CellArray) extends AnyVal {

  import Grid._

  def propagatedSignal(x: Int, y: Int)(implicit config: XinukConfig): GridPart = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
      cells.lift(x + i - 1).flatMap(_.lift(y + j - 1).map(_.smell))
    }

    val current = cells(x)(y)
    current match {
      case Obstacle => current
      case smelling: SmellMedium =>
        val currentSmell = current.smell
        val addends = SubcellCoordinates.map {
          case (i, j) if i == 1 || j == 1 =>
            destinationCellSignal(i, j).map(signal =>
              signal(i)(j) + signal(i + j - 1)(i + j - 1) + signal(i - j + 1)(j - i + 1)
            )
          case (i, j) =>
            destinationCellSignal(i, j).map(_.apply(i)(j))
        }
        val (newSmell, _) = addends.foldLeft(Array.ofDim[Signal](Cell.Size, Cell.Size), 0) { case ((cell, index), signalOpt) =>
          val (i, j) = SubcellCoordinates(index)
          cell(i)(j) = currentSmell(i)(j) + signalOpt.getOrElse(Signal.Zero) * config.signalSuppressionFactor
          (cell, index + 1)
        }
        newSmell(1)(1) = Signal.Zero
        smelling.withSmell(newSmell)
    }
  }
}

object Grid {
  type CellArray = Array[Array[GridPart]]

  def empty(bufferZone: Set[(Int, Int)], emptyCellFactory: => SmellingCell = EmptyCell.Instance)(implicit config: XinukConfig): Grid = {
    val n = config.gridSize
    val values = Array.tabulate[GridPart](n, n) {
      case (x, y) if bufferZone.contains((x, y)) => BufferCell(emptyCellFactory)
      case (x, y) if x == 0 || x == n - 1 || y == 0 || y == n - 1 => Obstacle
      case _ => emptyCellFactory
    }
    Grid(values)
  }

  val SubcellCoordinates: Vector[(Int, Int)] = {
    val pos = Vector(0, 1, 2)
    pos.flatMap(i => pos.collect { case j if !(i == 1 && j == 1) => (i, j) })
  }

  def neighbourCellCoordinates(x: Int, y: Int): Vector[(Int, Int)] = {
    val pos = Vector(-1, 0, 1)
    pos.flatMap(i => pos.collect {
      case j if !(i == 0 && j == 0) => (x + i, y + j)
    })
  }

}

final case class Signal(value: Double) extends AnyVal with Ordered[Signal] {
  def +(other: Signal) = Signal(value + other.value)

  def *(factor: Double) = Signal(value * factor)

  override def compare(that: Signal): Int = Ordering.Double.compare(value, that.value)
}

object Signal {
  final val Zero = Signal(0d)
}

final case class Energy(value: Double) extends AnyVal with Ordered[Energy] {
  override def compare(that: Energy): Int = Ordering.Double.compare(value, that.value)

  def -(other: Energy): Energy = Energy(value - other.value)

  def +(other: Energy): Energy = Energy(value + other.value)

  def unary_- : Energy = Energy(-value)
}

object Energy {
  final val Zero = Energy(0)
}

trait GridPart {
  def smell: SmellArray
}

trait SmellMedium extends GridPart {
  type Self <: SmellMedium

  import Cell._

  final def smellWith(added: Signal): SmellArray = smell + added

  def withSmell(smell: SmellArray): Self
}

trait SmellingCell extends SmellMedium {
  override type Self <: SmellingCell
}

object Cell {

  type SmellArray = Array[Array[Signal]]

  implicit class SmellArrayOps(private val arr: SmellArray) extends AnyVal {
    def +(other: SmellArray): SmellArray = {
      Array.tabulate(Cell.Size, Cell.Size)((x, y) => arr(x)(y) + other(x)(y))
    }

    def +(added: Signal): SmellArray = {
      Array.tabulate(Cell.Size, Cell.Size)((i, j) => arr(i)(j) + added)
    }
  }

  final val Size: Int = 3

  def emptySignal: SmellArray = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

case object Obstacle extends GridPart {
  override val smell: SmellArray = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

final case class BufferCell(cell: SmellingCell) extends SmellMedium with GridPart {

  override type Self = BufferCell

  override def smell: SmellArray = cell.smell

  override def withSmell(smell: SmellArray): BufferCell = copy(cell.withSmell(smell))
}

final case class EmptyCell(smell: SmellArray) extends SmellingCell {
  override type Self = EmptyCell

  override def withSmell(smell: SmellArray): EmptyCell = copy(smell)
}

object EmptyCell {
  final val Instance: EmptyCell = EmptyCell(Cell.emptySignal)
}