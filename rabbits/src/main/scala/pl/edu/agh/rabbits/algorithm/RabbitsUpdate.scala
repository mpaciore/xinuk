package pl.edu.agh.rabbits.algorithm

import pl.edu.agh.rabbits.model.Rabbit
import pl.edu.agh.xinuk.algorithm.Update

object RabbitsUpdate {

  case object CreateLettuce extends Update

  case object KeepLettuce extends Update


  case object CreateRabbit extends Update

  case class KeepRabbit(rabbit: Rabbit) extends Update

  case object KillRabbit extends Update

  case class AddRabbit(rabbit: Rabbit) extends Update

  case object RemoveRabbit extends Update
}
