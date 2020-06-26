package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.{Move, MovePlan, PlanCreator}
import pl.edu.agh.xinuk.model.{Direction, EmptyCell, Cell}

import scala.util.Random

object MockPlanCreator extends PlanCreator[MockConfig] {

  def randomMove(coords: (Int, Int), mock: MockCell, neighbours: Map[Direction.Direction, ((Int, Int), Cell)])
                (implicit config: MockConfig): MovePlan = {
    val availableDirections = neighbours.filter {
      case (_, (_, EmptyCell(_))) => true
      case _ => false
    }
    val direction = availableDirections.keys.toSeq(Random.nextInt(availableDirections.size))

    val action = Move(MockCell(config.mockInitialSignal), neighbours(direction)._1)
    val consequence = Move(EmptyCell(mock.smell), coords)
    MovePlan(action, consequence)
  }


  override def createPlans(coords: (Int, Int), cell: Cell, neighbours: Map[Direction.Direction, ((Int, Int), Cell)])
                          (implicit config: MockConfig): (Set[MovePlan], MockMetrics) = {
    cell match {
      case mock: MockCell => (Set(randomMove(coords, mock, neighbours)), MockMetrics(1, 0))
      case _ => (Set.empty, MockMetrics.empty)
    }
  }
}
