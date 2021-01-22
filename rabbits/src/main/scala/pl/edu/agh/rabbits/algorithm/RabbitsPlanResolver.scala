package pl.edu.agh.rabbits.algorithm

import pl.edu.agh.rabbits.algorithm.RabbitsUpdate._
import pl.edu.agh.rabbits.config.RabbitsConfig
import pl.edu.agh.rabbits.model.{Lettuce, Rabbit}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, Update}
import pl.edu.agh.xinuk.model.{CellContents, Empty}

final case class RabbitsPlanResolver() extends PlanResolver[RabbitsConfig] {
  override def isUpdateValid(iteration: Long, contents: CellContents, update: Update)(implicit config: RabbitsConfig): Boolean =
    (contents, update) match {
      case (_: Lettuce, KeepLettuce) => true
      case (_: Rabbit, _: KeepRabbit) => true

      case (_: Rabbit, KillRabbit) => true

      case (_: Rabbit, RemoveRabbit) => true

      case (_: Lettuce, _: AddRabbit) => true
      case (Empty, _: AddRabbit) => true

      case (_: Lettuce, CreateRabbit) => true
      case (Empty, CreateRabbit) => true

      case (_: Rabbit, CreateLettuce) => true
      case (Empty, CreateLettuce) => true

      case _ => false
    }

  override def applyUpdate(iteration: Long, contents: CellContents, update: Update)(implicit config: RabbitsConfig): (CellContents, RabbitsMetrics) = {
    val (newContents: CellContents, metrics: RabbitsMetrics) = (contents, update) match {
      case (lettuce: Lettuce, KeepLettuce) =>
        (Lettuce(lettuce.lifespan + 1), RabbitsMetrics.lettuce)
      case (_: Rabbit, KeepRabbit(rabbit)) =>
        (rabbit, RabbitsMetrics.rabbit(rabbit))

      case (rabbit: Rabbit, KillRabbit) =>
        (Empty, RabbitsMetrics.rabbitDeath(rabbit))

      case (_: Rabbit, RemoveRabbit) =>
        (Empty, RabbitsMetrics.empty)

      case (lettuce: Lettuce, AddRabbit(Rabbit(energy, lifespan))) =>
        val newRabbit = Rabbit(energy + config.lettuceEnergeticCapacity, lifespan)
        (newRabbit, RabbitsMetrics.rabbit(newRabbit) + RabbitsMetrics.lettuceConsumed(lettuce))
      case (Empty, AddRabbit(rabbit)) =>
        (rabbit, RabbitsMetrics.rabbit(rabbit))

      case (lettuce: Lettuce, CreateRabbit) =>
        val newRabbit = Rabbit(config.rabbitStartEnergy + config.lettuceEnergeticCapacity, 0)
        (newRabbit, RabbitsMetrics.rabbit(newRabbit) + RabbitsMetrics.rabbitReproduction + RabbitsMetrics.lettuceConsumed(lettuce))
      case (Empty, CreateRabbit) =>
        val newRabbit = Rabbit(config.rabbitStartEnergy, 0)
        (newRabbit, RabbitsMetrics.rabbit(newRabbit) + RabbitsMetrics.rabbitReproduction)

      case (Rabbit(energy, lifespan), CreateLettuce) =>
        val newRabbit = Rabbit(energy + config.lettuceEnergeticCapacity, lifespan)
        (newRabbit, RabbitsMetrics.rabbitAddedEnergy(config.lettuceEnergeticCapacity) + RabbitsMetrics.lettuceConsumed(Lettuce(0)))
      case (Empty, CreateLettuce) =>
        (Lettuce(0), RabbitsMetrics.lettuce)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")
    }

    (newContents, metrics)
  }
}
