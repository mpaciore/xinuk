package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig

import scala.math.Ordering


final case class Signal(value: Double) extends AnyVal with Ordered[Signal] {
  def +(other: Signal): Signal = Signal(value + other.value)

  def -(other: Signal): Signal = Signal(value - other.value)

  def *(factor: Double): Signal = Signal(value * factor)

  def /(divisor: Double): Signal = Signal(value / divisor)

  override def compare(that: Signal): Int = Ordering.Double.TotalOrdering.compare(value, that.value)
}

object Signal {
  private final val Zero = Signal(0d)

  def zero: Signal = Zero
}

final case class SignalMap(value: Map[Direction, Signal]) extends AnyVal {
  def apply(direction: Direction): Signal = value(direction)

  def +(other: SignalMap)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, this(d) + other(d))).toMap

  def +(added: Signal)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, this(d) + added)).toMap

  def -(other: SignalMap)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, this(d) - other(d))).toMap

  def -(deducted: Signal)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, this(d) - deducted)).toMap

  def *(factor: Double)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, this(d) * factor)).toMap

  def /(divisor: Double)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, this(d) / divisor)).toMap
}

object SignalMap {
  implicit def map2SignalMap(map: Map[Direction, Signal]): SignalMap =
    SignalMap(map)

  implicit def signalMap2Map(signalMap: SignalMap): Map[Direction, Signal] =
    signalMap.value

  def empty(implicit config: XinukConfig): SignalMap =
    uniform(Signal.zero)

  def uniform(initialSignal: Signal)(implicit config: XinukConfig): SignalMap =
    config.worldType.directions.map(d => (d, initialSignal)).toMap
}
