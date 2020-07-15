package pl.edu.agh.fortwist.model

import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.signal.SignalMap

final case class Foraminifera(energy: Energy, lifespan: Long)

object Foraminifera {
  def create()(implicit config: FortwistConfig): Foraminifera = Foraminifera(config.foraminiferaStartEnergy, 0)
}

final case class FortwistCell(signal: SignalMap, foraminiferas: Vector[Foraminifera], algae: Energy) extends GridPart {
  override type Self = FortwistCell

  override def withSignal(smell: SignalMap): FortwistCell = copy(signal = smell)
}

object FortwistCell {
  def create(foraminiferas: Vector[Foraminifera] = Vector.empty): FortwistCell =
    FortwistCell(SignalMap.empty, foraminiferas, Energy.Zero)
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