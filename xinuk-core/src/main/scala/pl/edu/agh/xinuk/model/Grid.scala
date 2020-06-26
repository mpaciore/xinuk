package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.Grid.CellArray

final case class Grid(cells: CellArray) extends AnyVal

object Grid {
  type CellArray = Array[Array[Cell]]

  def empty(emptyCellFactory: => Cell = EmptyCell.Instance)(implicit config: XinukConfig): Grid = {
    Grid(Array.tabulate[Cell](config.gridSize, config.gridSize)((_, _) => emptyCellFactory))
  }

  def neighbourCellCoordinates(x: Int, y: Int): Vector[(Int, Int)] = {
    val pos = Vector(-1, 0, 1)
    pos.flatMap(i => pos.collect {
      case j if !(i == 0 && j == 0) => (x + i, y + j)
    })
  }

}

final case class Signal(value: Double) extends AnyVal with Ordered[Signal] {
  def +(other: Signal): Signal = Signal(value + other.value)

  def -(other: Signal): Signal = Signal(value - other.value)

  def *(factor: Double): Signal = Signal(value * factor)

  def /(divisor: Double): Signal = Signal(value / divisor)

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

trait Cell {
  type Self <: Cell

  import Cell._

  def smell: SmellMap

  final def smellWith(added: Signal): SmellMap = smell + added

  final def smellWithout(deducted: Signal): SmellMap = smell - deducted

  final def smellWithout(deducted: SmellMap): SmellMap = smell - deducted

  def withSmell(smell: SmellMap): Self
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

case object Obstacle extends Cell {
  override type Self = Obstacle.type
  override val smell: SmellMap = Cell.emptySignal
  override def withSmell(smell: SmellMap): Obstacle.type = Obstacle
}

final case class BufferCell(cell: Cell) extends Cell {

  override type Self = BufferCell

  override def smell: SmellMap = cell.smell

  override def withSmell(smell: SmellMap): BufferCell = copy(cell.withSmell(smell))
}

final case class EmptyCell(smell: SmellMap) extends Cell {
  override type Self = EmptyCell

  override def withSmell(smell: SmellMap): EmptyCell = copy(smell)
}

object EmptyCell {
  final val Instance: EmptyCell = EmptyCell(Cell.emptySignal)
}