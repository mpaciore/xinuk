package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, Empty}

final case class MockPlanResolver() extends PlanResolver[MockConfig] {

  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: MockConfig): Boolean = {
    contents match {
      case Empty => true
      case _ => false
    }
  }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: MockConfig): (CellContents, Metrics) = {
    val newContents = update.value
    val metrics = update.value match {
      case Mock => MockMetrics(0, 1)
      case _ => MockMetrics.empty
    }
    (newContents, metrics)
  }
}
