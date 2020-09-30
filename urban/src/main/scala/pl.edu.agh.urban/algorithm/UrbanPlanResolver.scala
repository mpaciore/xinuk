package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.urban.model.UrbanCell
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellState, Empty}

final case class UrbanPlanResolver() extends PlanResolver[UrbanConfig] {

  override def isUpdateValid(state: CellState, update: StateUpdate)(implicit config: UrbanConfig): Boolean = {
    state.contents match {
      case Empty => true
      case _ => false
    }
  }

  override def applyUpdate(state: CellState, update: StateUpdate)(implicit config: UrbanConfig): (CellState, Metrics) = {
    val newState = update.value.withSignal(state.signalMap + update.value.signalMap)
    val metrics = update.value match {
      case CellState(_: UrbanCell, _) => UrbanMetrics(0, 1)
      case _ => UrbanMetrics.empty
    }
    (newState, metrics)
  }
}
