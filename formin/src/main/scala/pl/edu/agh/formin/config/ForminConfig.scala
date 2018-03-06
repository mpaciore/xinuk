package pl.edu.agh.formin.config

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{Energy, Signal}

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