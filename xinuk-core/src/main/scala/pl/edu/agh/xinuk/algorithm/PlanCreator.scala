package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellState, Direction}

trait PlanCreator[Config <: XinukConfig] {
  def createPlans(iteration: Long, cellState: CellState, neighbourStates: Map[Direction, CellState])
                 (implicit config: Config): (Map[Direction, Seq[Plan]], Metrics)
}
