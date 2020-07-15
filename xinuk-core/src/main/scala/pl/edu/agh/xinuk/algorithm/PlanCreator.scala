package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellState, Direction}
import pl.edu.agh.xinuk.simulation.Metrics

trait PlanCreator[Config <: XinukConfig] {
  def createPlans(cellState: CellState, neighbourStates: Map[Direction, CellState])(implicit config: Config): (Map[Direction, Seq[Plan]], Metrics)
}
