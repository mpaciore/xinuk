package pl.edu.agh.torch.model

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, CellState, Empty, SignalMap}

final case class Person(speed: Int) extends CellContents {
  override def passiveSignal()(implicit config: XinukConfig): SignalMap = SignalMap.uniform(config.asInstanceOf[TorchConfig].personInitialSignal)
}

trait PersonAccessible {
  def withPerson(person: Person): CellState
}

object PersonAccessible {

  def unapply(arg: CellState)(implicit config: TorchConfig): Option[PersonAccessible] = arg.contents match {
    case Empty => Some(unapplyEmpty(arg))
    case Exit => Some(unapplyExit(arg))
    case _ => None
  }

  def unapplyEmpty(arg: CellState)(implicit config: TorchConfig): PersonAccessible =
    new PersonAccessible {
      override def withPerson(person: Person): CellState = CellState(person, arg.signalMap + config.personInitialSignal)
    }

  def unapplyExit(arg: CellState): PersonAccessible =
    new PersonAccessible {
      override def withPerson(person: Person): CellState = CellState(Exit, arg.signalMap)
    }
}