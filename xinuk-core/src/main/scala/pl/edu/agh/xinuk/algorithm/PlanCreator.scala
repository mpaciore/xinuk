package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Direction.Direction
import pl.edu.agh.xinuk.model.Cell
import pl.edu.agh.xinuk.simulation.Metrics;

trait PlanCreator[T <: XinukConfig] {
  def createPlans(coords: (Int, Int), cell: Cell, neighbours: Map[Direction, ((Int, Int), Cell)])
                 (implicit config: T): (Set[MovePlan], Metrics)
}
