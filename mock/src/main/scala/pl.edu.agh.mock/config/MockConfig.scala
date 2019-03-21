package pl.edu.agh.mock.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class MockConfig(
                               mockInitialSignal: Signal,
                               signalSpeedRatio: Int,
                               signalSuppressionFactor: Double,
                               signalAttenuationFactor: Double,
                               gridSize: Int,
                               guiType: GuiType,
                               guiCellSize: Int,
                               workersRoot: Int,
                               iterationsNumber: Long,
                               isSupervisor: Boolean,
                               shardingMod: Int
                             ) extends XinukConfig