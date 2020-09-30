package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal, SignalMap}

case object Mock extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = config.asInstanceOf[MockConfig].mockInitialSignal
}
