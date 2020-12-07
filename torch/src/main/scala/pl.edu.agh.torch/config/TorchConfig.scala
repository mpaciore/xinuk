package pl.edu.agh.torch.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

final case class TorchConfig(worldType: WorldType,
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

                             guiCellSize: Int,
                             guiType: GuiType,
                             guiStartIteration: Long,
                             guiUpdateFrequency: Long,

                             spawnChance: Double,
                             personSpawnChance: Double,
                             fireSpawnChance: Double,
                             exitSpawnChance: Double,

                             personMaxSpeed: Int,
                             fireSpreadingFrequency: Int,

                             personInitialSignal: Signal,
                             fireInitialSignal: Signal,
                             exitInitialSignal: Signal
                            ) extends XinukConfig