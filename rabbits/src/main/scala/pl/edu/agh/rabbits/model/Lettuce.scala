package pl.edu.agh.rabbits.model

import pl.edu.agh.rabbits.config.RabbitsConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

final case class Lettuce(lifespan: Long) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    config.asInstanceOf[RabbitsConfig].lettuceInitialSignal
}
