package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, SignalMap}

case object Mock extends CellContents {
  override def passiveSignal()(implicit config: XinukConfig): SignalMap = SignalMap.uniform(config.asInstanceOf[MockConfig].mockInitialSignal)
}
