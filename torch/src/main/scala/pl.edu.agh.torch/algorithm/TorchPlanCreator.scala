package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.simulation.TorchMetrics
import pl.edu.agh.xinuk.algorithm.{MovePlan, PlanCreator}
import pl.edu.agh.xinuk.model.{Direction, Cell}

object TorchPlanCreator extends PlanCreator[TorchConfig] {
  override def createPlans(coords: (Int, Int), cell: Cell, neighbours: Map[Direction.Direction, ((Int, Int), Cell)])
                          (implicit config: TorchConfig): (Set[MovePlan], TorchMetrics) =
    ???
}
