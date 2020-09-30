package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._

trait PlanCreator[Config <: XinukConfig] {
  def initialize(worldShard: WorldShard)(implicit config: Config): Unit = ()

  def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                 (implicit config: Config): (Map[Direction, Seq[Plan]], Metrics)

  def finalize(worldShard: WorldShard)(implicit config: Config): Unit = ()
}
