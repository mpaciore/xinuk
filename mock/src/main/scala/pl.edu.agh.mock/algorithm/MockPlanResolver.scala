package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellState, Empty}
import pl.edu.agh.xinuk.simulation.Metrics

case class MockPlanResolver() extends PlanResolver[MockConfig] {

  import pl.edu.agh.xinuk.model.grid.GridDirection._

  override def isUpdateValid(state: CellState, update: StateUpdate)(implicit config: MockConfig): Boolean = {
    state match {
      case CellState(Empty, _) => true
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
