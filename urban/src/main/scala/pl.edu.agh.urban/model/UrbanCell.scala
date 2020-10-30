package pl.edu.agh.urban.model

import pl.edu.agh.urban.config.{TileType, UrbanConfig}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

import scala.collection.mutable.{Seq => MutableSeq}

case class UrbanCell(
                      tileType: TileType,
                      var entrance: Option[Entrance] = None,
                      var occupant: Option[Person] = None,
                      var markers: MutableSeq[PersonMarker] = MutableSeq.empty
                    ) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = {
    occupant match {
      case Some(_) => config.asInstanceOf[UrbanConfig].personSignal
      case None => Signal.zero
    }
  }

  override def signalFactor(iteration: Long)(implicit config: XinukConfig): Double = tileType.walkingFactor
}
