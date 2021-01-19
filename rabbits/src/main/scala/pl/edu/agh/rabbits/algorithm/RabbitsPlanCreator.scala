package pl.edu.agh.rabbits.algorithm

import pl.edu.agh.rabbits.algorithm.RabbitsUpdate._
import pl.edu.agh.rabbits.config.RabbitsConfig
import pl.edu.agh.rabbits.model.{Lettuce, Rabbit}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model._

final case class RabbitsPlanCreator() extends PlanCreator[RabbitsConfig] {

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: RabbitsConfig): (Plans, RabbitsMetrics) = {
    cellState.contents match {
      case _: Lettuce => lettucePlanning(iteration, neighbourContents)
      case rabbit: Rabbit => rabbitPlanning(rabbit, cellState.signalMap, neighbourContents)
      case _ => (Plans.empty, RabbitsMetrics.empty)
    }
  }

  private def lettucePlanning(iteration: Long, neighbourContents: Map[Direction, CellContents])
                             (implicit config: RabbitsConfig): (Plans, RabbitsMetrics) = {
    val ageLettucePlan = Plan(KeepLettuce)

    val spreadMap: Seq[(Option[Direction], Plan)] = if (iteration % config.lettuceReproductionFrequency == 0) {
      val availableDirections = neighbourContents.filter {
        case (_, Empty) => true
        case (_, _: Rabbit) => true
        case _ => false
      }.keys.toSeq
      if (availableDirections.nonEmpty) {
        val direction: Direction = randomDirection(availableDirections)
        val spreadLettucePlan = Plan(CreateLettuce)
        Seq(Some(direction) -> spreadLettucePlan)
      } else {
        Seq.empty
      }
    } else {
      Seq.empty
    }

    (Plans(spreadMap :+ (None -> ageLettucePlan)), RabbitsMetrics.empty)
  }

  private def rabbitPlanning(rabbit: Rabbit, signalMap: SignalMap, neighbourContents: Map[Direction, CellContents])
                            (implicit config: RabbitsConfig): (Plans, RabbitsMetrics) = {
    val plans = if (rabbit.energy < config.rabbitLifeActivityCost) {
      Plans(None -> Plan(KillRabbit))
    } else {
      val availableDirections = neighbourContents.filter {
        case (_, Empty) => true
        case (_, _: Lettuce) => true
        case _ => false
      }.keys.toSeq

      val agedRabbit = Rabbit(rabbit.energy - config.rabbitLifeActivityCost, rabbit.lifespan + 1)

      if (availableDirections.nonEmpty) {
        if (rabbit.energy > config.rabbitReproductionThreshold) {
          val direction: Direction = randomDirection(availableDirections)
          Plans(Some(direction) -> Plan(
            CreateRabbit,
            KeepRabbit(Rabbit(rabbit.energy - config.rabbitReproductionCost, rabbit.lifespan + 1)),
            KeepRabbit(agedRabbit)
          ))
        } else {
          val direction: Direction = availableDirections.map { direction => (direction, signalMap(direction)) }
            .maxBy(_._2)._1
          Plans(Some(direction) -> Plan(
            AddRabbit(agedRabbit),
            RemoveRabbit,
            KeepRabbit(agedRabbit)
          ))
        }
      } else {
        Plans(None -> Plan(KeepRabbit(agedRabbit)))
      }
    }

    (plans, RabbitsMetrics.empty)
  }

  private def randomDirection(directions: Seq[Direction])(implicit config: RabbitsConfig): Direction = {
    directions(config.random.nextInt(directions.size))
  }
}
