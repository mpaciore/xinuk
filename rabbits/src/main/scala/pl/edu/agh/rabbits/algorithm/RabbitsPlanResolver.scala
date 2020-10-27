package pl.edu.agh.rabbits.algorithm

import pl.edu.agh.rabbits.algorithm.RabbitsUpdateTag._
import pl.edu.agh.rabbits.config.RabbitsConfig
import pl.edu.agh.rabbits.model.{Lettuce, Rabbit}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, Empty}

final case class RabbitsPlanResolver() extends PlanResolver[RabbitsConfig] {
  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: RabbitsConfig): Boolean =
    (contents, update.updateTag, update.value) match {
      case (_: Lettuce, Stay, _: Lettuce) => true   // Lettuce can Stay in old cell
      case (_: Rabbit, Stay, _: Rabbit) => true     // Rabbit can Stay in old cell

      case (_: Rabbit, Die, Empty) => true          // Rabbit can Die

      case (_: Rabbit, Leave, Empty) => true        // Rabbit can Leave cell
      case (_: Lettuce, Arrive, _: Rabbit) => true  // Rabbit can Arrive into Lettuce (eat)
      case (Empty, Arrive, _: Rabbit) => true       // Rabbit can Arrive into Empty

      case (_: Lettuce, Spawn, _: Rabbit) => true   // Rabbit can Spawn into Lettuce (eat)
      case (Empty, Spawn, _: Rabbit) => true        // Rabbit can Spawn into Empty

      case (_: Rabbit, Spawn, _: Lettuce) => true   // Lettuce can Spawn into Rabbit (and immediately be eaten)
      case (Empty, Spawn, _: Lettuce) => true       // Lettuce can Spawn into Empty

      case _ => false                               // nothing else is allowed
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: RabbitsConfig): (CellContents, RabbitsMetrics) = {


    val (newContents: CellContents, metrics: RabbitsMetrics) = (contents, update.updateTag, update.value) match {
      case (_: Lettuce, Stay, lettuce: Lettuce) =>
        (lettuce, RabbitsMetrics.lettuce)
      case (_: Rabbit, Stay, rabbit: Rabbit) =>
        (rabbit, RabbitsMetrics.rabbit(rabbit))

      case (rabbit: Rabbit, Die, Empty) =>
        (Empty, RabbitsMetrics.rabbitDeath(rabbit))

      case (_: Rabbit, Leave, Empty) =>
        (Empty, RabbitsMetrics.empty)
      case (lettuce: Lettuce, Arrive, Rabbit(energy, lifespan)) =>
        val newRabbit = Rabbit(energy + config.lettuceEnergeticCapacity, lifespan)
        (newRabbit, RabbitsMetrics.rabbit(newRabbit) + RabbitsMetrics.lettuceConsumed(lettuce))
      case (Empty, Arrive, rabbit: Rabbit) =>
        (rabbit, RabbitsMetrics.rabbit(rabbit))

      case (lettuce: Lettuce, Spawn, Rabbit(energy, lifespan)) =>
        val newRabbit = Rabbit(energy + config.lettuceEnergeticCapacity, lifespan)
        (newRabbit, RabbitsMetrics.rabbit(newRabbit) + RabbitsMetrics.lettuceConsumed(lettuce) + RabbitsMetrics.rabbitReproduction)
      case (Empty, Spawn, rabbit: Rabbit) =>
        (rabbit, RabbitsMetrics.rabbit(rabbit) + RabbitsMetrics.rabbitReproduction)

      case (Rabbit(energy, lifespan), Spawn, lettuce: Lettuce) =>
        val newRabbit = Rabbit(energy + config.lettuceEnergeticCapacity, lifespan)
        (newRabbit, RabbitsMetrics.rabbit(newRabbit) + RabbitsMetrics.lettuceConsumed(lettuce))
      case (Empty, Spawn, lettuce: Lettuce) =>
        (lettuce, RabbitsMetrics.lettuce)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }

    (newContents, metrics)
  }
}
