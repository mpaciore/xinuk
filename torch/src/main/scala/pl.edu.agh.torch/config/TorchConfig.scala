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
                               humanInitialSignal: List[Signal],
                               humanPursuedSignalIndex: Int,
                               fireInitialSignal: List[Signal],
                               firePursuedSignalIndex: Int,
                               escapeInitialSignal: List[Signal],
                               escapePursuedSignalIndex: Int,
                               guiType: GuiType,
                               guiCellSize: Int,
                               workersRoot: Int,
                               iterationsNumber: Long,
                               isSupervisor: Boolean,
                               shardingMod: Int
                             ) extends XinukConfig