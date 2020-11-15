package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.algorithm.MockUpdate.{AddMock, RemoveMock}
import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, Update}
import pl.edu.agh.xinuk.model.{CellContents, Empty}

final case class MockPlanResolver() extends PlanResolver[MockConfig] {

  override def isUpdateValid(iteration: Long, contents: CellContents, update: Update)(implicit config: MockConfig): Boolean = {
    (contents, update) match {
      case (Empty, AddMock) => true
      case (Mock, RemoveMock) => true
      case _ => false
    }
  }

  override def applyUpdate(iteration: Long, contents: CellContents, update: Update)(implicit config: MockConfig): (CellContents, Metrics) = {
    val (newContents, metrics) = update match {
      case AddMock => (Mock, MockMetrics(0, 1))
      case RemoveMock => (Empty, MockMetrics.empty)
      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }
    (newContents, metrics)
  }
}
