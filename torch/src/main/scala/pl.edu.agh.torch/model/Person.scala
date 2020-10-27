package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, CellState, Empty, Signal, SignalMap}

final case class Person(speed: Int) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    config.asInstanceOf[TorchConfig].personInitialSignal
}
