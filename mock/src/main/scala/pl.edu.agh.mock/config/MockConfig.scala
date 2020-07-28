package pl.edu.agh.mock.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

final case class MockConfig(worldSize: Int,
                            worldType: WorldType,
                            iterationsNumber: Long,

                            signalSuppressionFactor: Double,
                            signalAttenuationFactor: Double,
                            signalSpeedRatio: Int,

                            workersRoot: Int,
                            isSupervisor: Boolean,
                            shardingMod: Int,

                            guiType: GuiType,
                            guiCellSize: Int,

                            mockInitialSignal: Signal
                           ) extends XinukConfig