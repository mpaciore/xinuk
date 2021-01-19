package pl.edu.agh.rabbits.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.{Signal, WorldType}

import java.security.SecureRandom
import scala.util.Random

/*
SC - spawn chance, SC ∈ [0,1] && SC ∈ R
RSC - rabbit spawn chance; RSC ∈ N.

RSSV - rabbit start signal value; RSSV ∈ [0,1] && RSSV ∈ R.
LSSV - lettuce start signal value; LSSV ∈ [0,1] && LSSV ∈ R.

RSE - rabbit start energy; RSE ∈ [0,1] && RSE ∈ R.
RRC - rabbit reproduction cost; RRC ∈ [0,1] && RRC ∈ R.
RRT - rabbit reproduction threshold; RRT ∈ [0,1] && RRT ∈ R.
RLAC - rabbit life activities(vegetation and movement) cost; RLAC ∈ [0,1] && RLAC ∈ R.

LRF - lettuce reproduction frequency; LRF ∈ N.
LEC - lettuce energetic capacity; LEC ∈ [0,1] && LEC ∈ R.
 */

final case class RabbitsConfig(
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

                                spawnChance: Double,
                                rabbitSpawnChance: Double,

                                rabbitInitialSignal: Signal,
                                lettuceInitialSignal: Signal,

                                rabbitStartEnergy: Double,
                                rabbitReproductionCost: Double,
                                rabbitReproductionThreshold: Double,
                                rabbitLifeActivityCost: Double,

                                lettuceEnergeticCapacity: Double,
                                lettuceReproductionFrequency: Int,
                              ) extends XinukConfig {
  val random: Random = new SecureRandom
}
