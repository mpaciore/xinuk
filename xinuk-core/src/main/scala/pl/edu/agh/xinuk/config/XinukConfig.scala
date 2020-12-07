package pl.edu.agh.xinuk.config

import com.avsystem.commons.misc.{AbstractNamedEnumCompanion, NamedEnum}
import pl.edu.agh.xinuk.model.WorldType

trait XinukConfig {
  def worldType: WorldType
  def worldWidth: Int
  def worldHeight: Int
  def iterationsNumber: Long
  def iterationFinishedLogFrequency: Long
  def skipEmptyLogs: Boolean

  def signalSuppressionFactor: Double
  def signalAttenuationFactor: Double
  def signalDisabled: Boolean

  def workersX: Int
  def workersY: Int
  def isSupervisor: Boolean
  def shardingMod: Int

  def guiType: GuiType
  def guiCellSize: Int
  def guiStartIteration: Long
  def guiUpdateFrequency: Long
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

  case object Snapshot extends GuiType {
    override def name: String = "snapshot"
  }
}
