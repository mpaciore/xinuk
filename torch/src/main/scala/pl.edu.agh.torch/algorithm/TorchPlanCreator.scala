package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.simulation.TorchMetrics
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator}
import pl.edu.agh.xinuk.model.GridPart
import pl.edu.agh.xinuk.model.grid.GridDirection

object TorchPlanCreator extends PlanCreator[TorchConfig] {
  override def createPlans(coords: (Int, Int), cell: GridPart, neighbours: Map[GridDirection.Direction, ((Int, Int), GridPart)])
                          (implicit config: TorchConfig): (Set[Plan], TorchMetrics) =
    ???
}
