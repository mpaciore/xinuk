package pl.edu.agh.mock.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

final case class MockConfig(worldType: WorldType,
                            worldWidth: Int,
                            worldHeight: Int,
                            iterationsNumber: Long,
                            iterationFinishedLogFrequency: Long,

                            signalSuppressionFactor: Double,
                            signalAttenuationFactor: Double,
                            signalSpeedRatio: Int,

                            workersX: Int,
                            workersY: Int,
                            isSupervisor: Boolean,
                            shardingMod: Int,

                            guiType: GuiType,
                            guiCellSize: Int,
                            guiUpdateFrequency: Long,

                            mockInitialSignal: Signal
                           ) extends XinukConfig