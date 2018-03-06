package pl.edu.agh.formin.config

import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Energy, Signal}

import scala.util.Try

/*
FSE - foraminifera start energy; FSE ∈ [0,1] && FSE ∈ R.
FRC - foraminifera reproduction cost; FRC ∈ [0,1] && FRC ∈ R.
FRT - foraminifera reproduction threshold; FRT ∈ [0,1] && FRT ∈ R.
FLAC - foraminifera life activities(vegetation and movement) cost; FLAC ∈ [0,1] && FLAC ∈ R.
ARF - algae reproduction frequency; ARF ∈ N.
AEC - algae energetic capacity; AEC ∈ [0,1] && AEC ∈ R.
SSR - signal speed ratio; SSR ∈ N. Foraminifera speed is 1.
DFF - diffraction factor; DFF ∈ [0,1] && DFF ∈ R.
SPF - global suppression factor of the signal; SPF ∈ [0,1] && SPF ∈ R.
GS - grid size; GS ∈ N, where map size is GSxGS.
SC - spawn chance, SC ∈ [0,1] && SC ∈ R
FSC - foraminifera spawn chance; FAR ∈ N.
FSSV - foraminifera start signal value; FSSV ∈ [0,1] && FSSV ∈ R.
ASSV - algae start signal value; ASSV ∈ [0,1] && ASSV ∈ R.
 */

final case class ForminConfig(
                               foraminiferaStartEnergy: Energy,
                               foraminiferaReproductionCost: Energy,
                               foraminiferaReproductionThreshold: Energy,
                               foraminiferaLifeActivityCost: Energy,
                               algaeReproductionFrequency: Int,
                               algaeEnergeticCapacity: Energy,
                               signalSpeedRatio: Int,
                               //diffractionFactor: Double,
                               signalSuppressionFactor: Double,
                               gridSize: Int,
                               spawnChance: Double,
                               foraminiferaSpawnChance: Double,
                               foraminiferaInitialSignal: Signal,
                               algaeInitialSignal: Signal,
                               guiType: GuiType,
                               guiCellSize: Int,
                               workersRoot: Int,
                               iterationsNumber: Long,
                               isSupervisor: Boolean,
                               shardingMod: Int
                             ) extends XinukConfig

object ForminConfig {

  implicit val signalReader: ValueReader[Signal] =
    (config: Config, path: String) => Signal(config.getNumber(path).doubleValue())

  implicit val energyReader: ValueReader[Energy] =
    (config: Config, path: String) => Energy(config.getNumber(path).doubleValue())

  implicit val guiTypeReader: ValueReader[GuiType] =
    (config: Config, path: String) => GuiType.byName(config.getString(path))


  def fromConfig(config: Config): Try[ForminConfig] = {
    import net.ceedubs.ficus.Ficus._
    import net.ceedubs.ficus.readers.ArbitraryTypeReader._
    Try(config.as[ForminConfig]("config"))
  }
}