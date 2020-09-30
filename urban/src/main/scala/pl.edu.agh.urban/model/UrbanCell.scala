package pl.edu.agh.urban.model

import pl.edu.agh.urban.config.{TileType, UrbanConfig}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

import scala.collection.mutable.{Seq => MutableSeq}

case class UrbanCell(tileType: TileType, var occupant: Option[Person] = Option.empty, var markers: MutableSeq[PersonMarker] = MutableSeq.empty) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = {
    occupant
      .map(_ => config.asInstanceOf[UrbanConfig].personSignal)
      .getOrElse(Signal.zero)

    Signal.zero // todo: debug
  }

  override def signalFactor(iteration: Long)(implicit config: XinukConfig): Double = tileType.walkingFactor
}
