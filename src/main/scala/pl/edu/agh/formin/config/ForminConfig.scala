package pl.edu.agh.formin.config

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.typesafe.config.Config
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.formin.model.{Energy, Signal}

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

final case class ForminConfig private(
                                       foraminiferaStartEnergy: Energy,
                                       foraminiferaReproductionCost: Energy,
                                       foraminiferaReproductionThreshold: Energy,
                                       foraminiferaLifeActivityCost: Energy,
                                       algaeReproductionFrequency: Int,
                                       algaeEnergeticCapacity: Energy,
                                       signalSpeedRatio: Int,
                                       //diffractionFactor: Double,
                                       signalSuppresionFactor: Double,
                                       gridSize: Int,
                                       spawnChance: Double,
                                       foraminiferaSpawnChance: Double,
                                       foraminiferaInitialSignal: Signal,
                                       algaeInitialSignal: Signal,
                                       guiType: GuiType,
                                       guiCellSize: Int
                 )

object ForminConfig {
  private implicit val energyReader = new ValueReader[Energy] {
    override def read(config: Config, path: String): Energy = {
      Energy(config.getNumber(path).doubleValue())
    }
  }

  private implicit val signalReader = new ValueReader[Signal] {
    override def read(config: Config, path: String): Signal = {
      Signal(config.getNumber(path).doubleValue())
    }
  }

  private implicit val guiTypeReader = new ValueReader[GuiType] {
    override def read(config: Config, path: String): GuiType = {
      GuiType.byName(config.getString(path))
    }
  }

  def fromConfig(config: Config): Try[ForminConfig] = {
    import net.ceedubs.ficus.Ficus._
    import net.ceedubs.ficus.readers.ArbitraryTypeReader._
    Try(config.as[ForminConfig]("config"))
  }
}

sealed trait GuiType extends NamedEnum

object GuiType extends NamedEnumCompanion[GuiType] {

  case object None extends GuiType {
    override val name: String = "none"
  }

  case object Basic extends GuiType {
    override def name: String = "basic"
  }

  case object Signal extends GuiType {
    override def name: String = "signal"
  }

  override val values: List[GuiType] = caseObjects
}