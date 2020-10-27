package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{Exit, Fire, Person}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans, StateUpdate}
import pl.edu.agh.xinuk.model._

import scala.util.Random

final case class TorchPlanCreator() extends PlanCreator[TorchConfig] {

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: TorchConfig): (Plans, TorchMetrics) = {
    cellState.contents match {
      case Fire => (spreadFire(iteration, neighbourContents), TorchMetrics.fire)
      case person: Person => (movePerson(iteration, person, cellState.signalMap, neighbourContents), TorchMetrics.person)
      case Exit => (Plans.empty, TorchMetrics.exit)
      case _ => (Plans.empty, TorchMetrics.empty)
    }
  }

  private def spreadFire(iteration: Long, neighbourContents: Map[Direction, CellContents])
                        (implicit config: TorchConfig): Plans = {
    if (iteration % config.fireSpreadingFrequency == 0) {
      val availableDirections: Seq[Direction] = neighbourContents.filterNot(_._2 == Obstacle).keys.toSeq
      val targetDirection = availableDirections(Random.nextInt(availableDirections.size))
      Plans(Map((targetDirection, Seq(Plan(StateUpdate(Fire))))))
    } else {
      Plans.empty
    }
  }


  private def movePerson(iteration: Long, person: Person, cellSignal: SignalMap, neighbourContents: Map[Direction, CellContents])
                        (implicit config: TorchConfig): Plans = {
    if (iteration % person.speed == 0) {
      val directions =
        Random.shuffle(cellSignal.filter { case (direction, _) => neighbourContents.contains(direction) &&
          (neighbourContents(direction) == Empty || neighbourContents(direction) == Exit) }.toSeq)
        .sortBy(_._2)(Ordering[Signal].reverse)
        .map(_._1)

      if (directions.nonEmpty) {
        Plans(Map((directions.head, Seq(Plan(StateUpdate(person), StateUpdate(Empty))))))
      } else {
        Plans.empty
      }

    } else {
      Plans.empty
    }
  }
}