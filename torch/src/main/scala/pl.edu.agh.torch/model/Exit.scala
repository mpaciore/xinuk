package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, CellState, Empty, Signal, SignalMap}

case object Exit extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = config.asInstanceOf[TorchConfig].exitInitialSignal
}

trait ExitAccessible {
  def withExit(): CellState
}

object ExitAccessible {

  def unapply(arg: CellState)(implicit config: TorchConfig): Option[ExitAccessible] = arg.contents match {
    case Empty => Some(unapplyEmpty(arg))
    case _ => None
  }

  def unapplyEmpty(arg: CellState)(implicit config: TorchConfig): ExitAccessible =
    new ExitAccessible {
      override def withExit(): CellState = CellState(Exit, arg.signalMap + config.exitInitialSignal)
    }
}
