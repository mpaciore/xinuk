package pl.edu.agh.fortwist.model

import java.util.UUID

import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._

final case class Seabed(foraminiferas: Seq[Foraminifera], algae: Double) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = {
    config.asInstanceOf[FortwistConfig].foraminiferaInitialSignal * foraminiferas.size +
      config.asInstanceOf[FortwistConfig].algaeSignalMultiplier * algae
  }
}

object Seabed {
  def apply(foraminiferas: Seq[Foraminifera], algae: Double): Seabed = new Seabed(foraminiferas, math.min(1.0, algae))

  def apply(foraminiferas: Seq[Foraminifera] = Seq.empty): Seabed = Seabed(foraminiferas, 0)

  def algaeGrowth(currentValue: Double)(implicit config: FortwistConfig): Double =
    math.sqrt(currentValue) * config.algaeRegenerationRate
}

final case class Foraminifera(energy: Double, lifespan: Long, id: UUID = UUID.randomUUID()) {
  def changed(energyDiff: Double, lifespanDiff: Long = 1L): Foraminifera =
    Foraminifera(energy + energyDiff, lifespan + lifespanDiff, id)
}

object Foraminifera {
  def apply()(implicit config: FortwistConfig): Foraminifera = Foraminifera(config.foraminiferaStartEnergy, 0)
}
