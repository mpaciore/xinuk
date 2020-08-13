package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, CellState, Direction}

trait PlanCreator[Config <: XinukConfig] {
  def createPlans(iteration: Long, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                 (implicit config: Config): (Map[Direction, Seq[Plan]], Metrics)
}
