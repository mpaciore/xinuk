package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, CellState, Empty, Signal, SignalMap}

case object Fire extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = config.asInstanceOf[TorchConfig].fireInitialSignal
}

trait FireAccessible {
  def withFire(): CellState
}

object FireAccessible {

  def unapply(arg: CellState)(implicit config: TorchConfig): Option[FireAccessible] = arg.contents match {
    case Empty => Some(unapplyAny(arg))
    case Exit => Some(unapplyAny(arg))
    case Person(_) => Some(unapplyAny(arg))
    case _ => None
  }

  def unapplyAny(arg: CellState)(implicit config: TorchConfig): FireAccessible =
    new FireAccessible {
      override def withFire(): CellState = CellState(Fire, arg.signalMap + config.fireInitialSignal)
    }
}