package pl.edu.agh.formin.model

import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.Grid.{CellArray, SmellArray}

import scala.collection.mutable

final case class Grid(cells: CellArray) {

  import Grid._

  private val cellSignalFun: (Int) => (Int) => Option[SmellArray] = {
    cells.map(_.lift).lift.andThen {
      case Some(rowFunction) => rowFunction.andThen(_.map(_.smell))
      case None => _ => None
    }
  }

  def propagatedSignal(x: Int, y: Int)(implicit config: ForminConfig): GridPart = {
    @inline def destinationCellSignal(i: Int, j: Int): Option[SmellArray] = {
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
        val (newSmell, _) = addends.foldLeft(Array.ofDim[Signal](Cell.Size, Cell.Size), 0) { case ((cell, index), signalOpt) =>
          val (i, j) = SubcellCoordinates(index)
          cell(i)(j) = currentSmell(i)(j) + signalOpt.getOrElse(Signal.Zero) * config.signalSuppresionFactor
          (cell, index + 1)
        }
        newSmell(1)(1) = Signal.Zero
        smelling.withSmell(newSmell)
    }
  }
}

object Grid {
  type CellArray = Array[Array[GridPart]]

  //todo is not mutable after all
  type SmellArray = Array[Array[Signal]]

  def empty(implicit config: ForminConfig): Grid = {
    val n = config.gridSize
    val values = Array.tabulate[GridPart](n, n) {
      case (x, y) if x == 0 || x == n - 1 || y == 0 || y == n - 1 => Obstacle
      case _ => EmptyCell.Instance
    }
    Grid(values)
  }

  val SubcellCoordinates: Vector[(Int, Int)] = {
    val pos = Vector(0, 1, 2)
    pos.flatMap(i => pos.collect { case j if !(i == 1 && j == 1) => (i, j) })
  }

  private val neighbourCoordinatesMap: mutable.Map[(Int, Int), Vector[(Int, Int)]] = mutable.Map.empty

  def neighbourCoordinates(x: Int, y: Int): Vector[(Int, Int)] = {
    neighbourCoordinatesMap.getOrElseUpdate((x, y), calculateNeighbourCoordinates(x, y))
  }

  private def calculateNeighbourCoordinates(x: Int, y: Int): Vector[(Int, Int)] = {
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
  val Zero = Signal(0d)
}

final case class Energy(value: Double) extends AnyVal with Ordered[Energy] {
  override def compare(that: Energy): Int = Ordering.Double.compare(value, that.value)

  def -(other: Energy): Energy = Energy(value - other.value)

  def +(other: Energy): Energy = Energy(value + other.value)
}

sealed trait GridPart {
  def smell: SmellArray
}

sealed trait Cell extends GridPart

object Cell {
  final val Size = 3

  def emptySignal: SmellArray = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

sealed trait SmellMedium[T <: SmellMedium[T]] extends GridPart {

  protected final def smellWith(added: Signal): SmellArray = {
    Array.tabulate(Cell.Size, Cell.Size)((i, j) => smell(i)(j) + added)
  }

  def withSmell(smell: SmellArray): T

}

sealed trait HasEnergy {
  def energy: Energy
}

sealed trait ForaminiferaAcessible[ResultGridPart <: GridPart] extends GridPart {
  def withForaminifera(energy: Energy)(implicit config: ForminConfig): ResultGridPart
}

sealed trait AlgaeAccessible[ResultGridPart <: GridPart] extends GridPart {
  def withAlgae(implicit config: ForminConfig): ResultGridPart
}

final case class ForaminiferaCell(energy: Energy, smell: SmellArray)
  extends Cell with HasEnergy with SmellMedium[ForaminiferaCell] {

  override def withSmell(smell: SmellArray): ForaminiferaCell = copy(smell = smell)
}

final case class AlgaeCell(smell: SmellArray)
  extends Cell with SmellMedium[AlgaeCell] with ForaminiferaAcessible[ForaminiferaCell] {

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)

  def withForaminifera(energy: Energy)(implicit config: ForminConfig): ForaminiferaCell = {
    ForaminiferaCell(energy + config.algaeEnergeticCapacity, smellWith(config.foraminiferaInitialSignal))
  }
}

case object Obstacle extends Cell {
  override val smell: SmellArray = Array.fill(Cell.Size, Cell.Size)(Signal.Zero)
}

final case class BufferCell(cell: SmellMedium[T] forSome {type T <: Cell with SmellMedium[T]})
  extends GridPart
    with SmellMedium[BufferCell]
    with AlgaeAccessible[BufferCell]
    with ForaminiferaAcessible[BufferCell] {

  override def smell: SmellArray = cell.smell

  override def withSmell(smell: SmellArray): BufferCell = {
    BufferCell(cell.withSmell(smell))
  }

  override def withAlgae(implicit config: ForminConfig): BufferCell = {
    BufferCell(AlgaeCell(smellWith(config.algaeInitialSignal)))
  }

  override def withForaminifera(energy: Energy)(implicit config: ForminConfig): BufferCell = {
    BufferCell(ForaminiferaCell(energy, smellWith(config.foraminiferaInitialSignal)))
  }

}

final case class EmptyCell(smell: SmellArray = Cell.emptySignal)
  extends Cell with SmellMedium[EmptyCell] with AlgaeAccessible[AlgaeCell] with ForaminiferaAcessible[ForaminiferaCell] {

  override def withSmell(smell: SmellArray): EmptyCell = copy(smell = smell)

  def withForaminifera(energy: Energy)(implicit config: ForminConfig): ForaminiferaCell = {
    ForaminiferaCell(energy, smellWith(config.foraminiferaInitialSignal))
  }

  def withAlgae(implicit config: ForminConfig): AlgaeCell = {
    AlgaeCell(smellWith(config.algaeInitialSignal))
  }

}

object EmptyCell {
  final val Instance = EmptyCell()
}