package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.algorithm.MockUpdate.{AddMock, RemoveMock}
import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model.{CellContents, CellId, CellState, Direction, Empty}

import scala.util.Random

final case class MockPlanCreator() extends PlanCreator[MockConfig] {

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: MockConfig): (Plans, MockMetrics) = {
    cellState.contents match {
      case Mock => (randomMove(neighbourContents), MockMetrics(1, 0))
      case _ => (Plans.empty, MockMetrics.empty)
    }
  }

  def randomMove(neighbours: Map[Direction, CellContents])(implicit config: MockConfig): Plans = {
    val availableDirections = neighbours.filter {
      case (_, Empty) => true
      case _ => false
    }

    if (availableDirections.isEmpty) {
      Plans.empty
    } else {

      val direction = availableDirections.keys.toSeq(Random.nextInt(availableDirections.size))

      Plans(Some(direction) -> Plan(AddMock, RemoveMock))
    }
  }
}