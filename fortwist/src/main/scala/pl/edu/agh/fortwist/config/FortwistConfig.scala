package pl.edu.agh.fortwist.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

final case class FortwistConfig(
                                 worldType: WorldType,
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

                                 foraminiferaSpawnChance: Double,

                                 foraminiferaInitialSignal: Signal,
                                 algaeSignalMultiplier: Signal,

                                 foraminiferaStartEnergy: Double,
                                 foraminiferaReproductionCost: Double,
                                 foraminiferaReproductionThreshold: Double,
                                 foraminiferaLifeActivityCost: Double,

                                 algaeStartEnergy: Double,
                                 algaeRegenerationRate: Double,
                                 algaeEnergeticCapacity: Double
                               ) extends XinukConfig