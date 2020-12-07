package pl.edu.agh.mock.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

final case class MockConfig(worldType: WorldType,
                            worldWidth: Int,
                            worldHeight: Int,
                            iterationsNumber: Long,
                            iterationFinishedLogFrequency: Long,
                            skipEmptyLogs: Boolean,

                            signalSuppressionFactor: Double,
                            signalAttenuationFactor: Double,
                            signalDisabled: Boolean,

                            workersX: Int,
                            workersY: Int,
                            isSupervisor: Boolean,
                            shardingMod: Int,

                            guiType: GuiType,
                            guiCellSize: Int,
                            guiStartIteration: Long,
                            guiUpdateFrequency: Long,

                            mockInitialSignal: Signal
                           ) extends XinukConfig