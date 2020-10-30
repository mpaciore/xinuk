package pl.edu.agh.urban.model

import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

case class SignalSourceCell() extends CellContents{
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = {
    config.asInstanceOf[UrbanConfig].targetSignal
  }
}
