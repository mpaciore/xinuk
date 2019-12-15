package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.Grid.CellArray

final case class Grid(cells: CellArray) extends AnyVal {

  import Grid._

  // TODO ???
//  def propagatedSignal(calculateSmellAddends: (CellArray, Int, Int) => Vector[Option[Signal]], x: Int, y: Int)(implicit config: XinukConfig): GridPart = {
//    val current = cells(x)(y)
//    current match {
//      case Obstacle => current
//      case smelling: SmellMedium =>
//        val currentSmell = current.smell
//        val addends = calculateSmellAddends(cells, x, y)
//        val (newSmell, _) = addends.foldLeft(Array.ofDim[Signal](Cell.Size, Cell.Size), 0) { case ((cell, index), signalOpt) =>
//          val (i, j) = Cell.subCellCoordinates(index)
//          cell(i)(j) = (currentSmell(i)(j) * config.signalAttenuationFactor) + (signalOpt.getOrElse(Signal.Zero) * config.signalSuppressionFactor)
//          (cell, index + 1)
//        }
//        newSmell(1)(1) = Signal.Zero
//        smelling.withSmell(newSmell)
//    }
//  }
}

object Grid {
  type CellArray = Array[Array[GridPart]]

  def empty(emptyCellFactory: => SmellingCell = EmptyCell.Instance)(implicit config: XinukConfig): Grid = {
    Grid(Array.tabulate[GridPart](config.gridSize, config.gridSize)((_, _) => emptyCellFactory))
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

  def -(other: Signal) = Signal(value - other.value)

  def *(factor: Double) = Signal(value * factor)

  def /(divisor: Double) = Signal(value / divisor)

  override def compare(that: Signal): Int = Ordering.Double.compare(value, that.value)
}

object Signal {
  final val Zero = Signal(0d)
}

final case class Energy(value: Double) extends AnyVal with Ordered[Energy] {
  override def compare(that: Energy): Int = Ordering.Double.compare(value, that.value)

  def -(other: Energy): Energy = Energy(value - other.value)

  def +(other: Energy): Energy = Energy(value + other.value)

  def *(factor: Double): Energy = Energy(value * factor)

  def unary_- : Energy = Energy(-value)
}

object Energy {
  final val Zero = Energy(0)
}

trait GridPart {
  def smell: SmellMap
}

trait SmellMedium extends GridPart {
  type Self <: SmellMedium

  import Cell._

  final def smellWith(added: Signal): SmellMap = smell + added

  final def smellWithout(deducted: Signal): SmellMap = smell - deducted

  final def smellWithout(deducted: SmellMap): SmellMap = smell - deducted

  def withSmell(smell: SmellMap): Self
}

trait SmellingCell extends SmellMedium {
  override type Self <: SmellingCell
}

object Cell {

  type SmellMap = Map[Direction, Signal]

  implicit class SmellMapOps(private val smell: SmellMap) extends AnyVal {
    def +(other: SmellMap): SmellMap = Direction.values.map(d => (d, smell(d) + other(d))).toMap

    def +(added: Signal): SmellMap = Direction.values.map(d => (d, smell(d) + added)).toMap

    def -(other: SmellMap): SmellMap = Direction.values.map(d => (d, smell(d) - other(d))).toMap

    def -(deducted: Signal): SmellMap = Direction.values.map(d => (d, smell(d) - deducted)).toMap

    def *(factor: Double): SmellMap = Direction.values.map(d => (d, smell(d) * factor)).toMap

    def /(divisor: Double): SmellMap = Direction.values.map(d => (d, smell(d) / divisor)).toMap
  }

  def uniformSignal(initialSignal: => Signal): SmellMap = Direction.values.map(d => (d, initialSignal)).toMap

  def emptySignal: SmellMap = uniformSignal(Signal.Zero)
}

case object Obstacle extends GridPart {
  override val smell: SmellMap = Cell.emptySignal
}

final case class BufferCell(cell: SmellingCell) extends SmellMedium with GridPart {

  override type Self = BufferCell

  override def smell: SmellMap = cell.smell

  override def withSmell(smell: SmellMap): BufferCell = copy(cell.withSmell(smell))
}

final case class EmptyCell(smell: SmellMap) extends SmellingCell {
  override type Self = EmptyCell

  override def withSmell(smell: SmellMap): EmptyCell = copy(smell)
}

object EmptyCell {
  final val Instance: EmptyCell = EmptyCell(Cell.emptySignal)
}