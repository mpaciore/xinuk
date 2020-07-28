package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellState, Empty}

final case class MockPlanResolver() extends PlanResolver[MockConfig] {

  override def isUpdateValid(state: CellState, update: StateUpdate)(implicit config: MockConfig): Boolean = {
    state.contents match {
      case Empty => true
      case _ => false
    }
  }

  override def applyUpdate(state: CellState, update: StateUpdate)(implicit config: MockConfig): (CellState, Metrics) = {
    val newState = update.value.withSignal(state.signalMap + update.value.signalMap)
    val metrics = update.value match {
      case CellState(Mock, _) => MockMetrics(0, 1)
      case _ => MockMetrics.empty
    }
    (newState, metrics)
  }
}
