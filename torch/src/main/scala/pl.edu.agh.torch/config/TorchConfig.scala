package pl.edu.agh.torch.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class TorchConfig(

                               humanMaxSpeed: Int,
                               fireSpeadingFrequency: Int,
                               signalSpeedRatio: Int,
                               signalSuppressionFactor: Double,
                               signalAttenuationFactor: Double,
                               gridSize: Int,
                               spawnChance: Double,
                               humanSpawnChance: Double,
                               fireSpawnChance: Double,
                               escapeSpawnChance: Double,
                               humanInitialSignal: Signal,
                               fireInitialSignal: Signal,
                               escapeInitialSignal: Signal,
                               guiType: GuiType,
                               guiCellSize: Int,
                               workersRoot: Int,
                               iterationsNumber: Long,
                               isSupervisor: Boolean,
                               shardingMod: Int
                             ) extends XinukConfig