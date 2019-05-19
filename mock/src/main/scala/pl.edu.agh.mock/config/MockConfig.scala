package pl.edu.agh.mock.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class MockConfig(
                             gridSize: Int,
                             guiCellSize: Int,
                             signalSuppressionFactor: Double,
                             signalAttenuationFactor: Double,
                             workersRoot: Int,
                             shardingMod: Int,

                             guiType: GuiType,
                             isSupervisor: Boolean,
                             signalSpeedRatio: Int,
                             iterationsNumber: Long,

                             mockInitialSignal: List[Signal],
                             mockPursuedSignalIndex: Int
                           ) extends XinukConfig