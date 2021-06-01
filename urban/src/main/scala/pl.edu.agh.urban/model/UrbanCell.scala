package pl.edu.agh.urban.model

import pl.edu.agh.urban.config.{TileType, UrbanConfig}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

case class UrbanCell(
                      tileType: TileType,
                      entrance: Option[Entrance] = None,
                      occupant: Option[Person] = None,
                      markers: Seq[PersonMarker] = Seq.empty
                    ) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal = {
    occupant match {
      case Some(_) => config.asInstanceOf[UrbanConfig].personSignal
      case None => Signal.zero
    }
  }

  override def signalFactor(iteration: Long)(implicit config: XinukConfig): Double = tileType.walkingFactor

  def isWalkable: Boolean = tileType.walkingFactor > 0.0
}
