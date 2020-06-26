package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.simulation.TorchMetrics
import pl.edu.agh.xinuk.algorithm.PlanResolver
import pl.edu.agh.xinuk.model.Cell
import pl.edu.agh.xinuk.simulation.Metrics

object TorchPlanResolver extends PlanResolver[TorchMetrics] {
  override def isUpdateValid(update: Cell, target: Cell)(implicit config: TorchMetrics): Boolean =
    ???

  override def applyUpdate(update: Cell, target: Cell)(implicit config: TorchMetrics): (Cell, Metrics) =
    ???
}
