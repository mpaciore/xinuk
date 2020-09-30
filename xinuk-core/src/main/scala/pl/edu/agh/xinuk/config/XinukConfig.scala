package pl.edu.agh.xinuk.config

import com.avsystem.commons.misc.{AbstractNamedEnumCompanion, NamedEnum}
import pl.edu.agh.xinuk.model.WorldType

trait XinukConfig {
  def worldType: WorldType
  def worldWidth: Int
  def worldHeight: Int
  def iterationsNumber: Long

  def signalSuppressionFactor: Double
  def signalAttenuationFactor: Double
  def signalSpeedRatio: Int

  def workersRoot: Int
  def isSupervisor: Boolean
  def shardingMod: Int

  def guiCellSize: Int
  def guiType: GuiType
}

sealed trait GuiType extends NamedEnum

object GuiType extends AbstractNamedEnumCompanion[GuiType] {

  override val values: List[GuiType] = caseObjects

  case object None extends GuiType {
    override val name: String = "none"
  }

  case object Grid extends GuiType {
    override def name: String = "grid"
  }
}
