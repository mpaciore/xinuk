package pl.edu.agh.rabbits.model

import pl.edu.agh.rabbits.config.RabbitsConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._

final case class Rabbit(energy: Double, lifespan: Long) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    config.asInstanceOf[RabbitsConfig].rabbitInitialSignal
}
