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