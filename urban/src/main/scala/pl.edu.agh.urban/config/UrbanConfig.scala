package pl.edu.agh.urban.config

import java.awt.Color
import java.awt.image.BufferedImage
import java.time.LocalTime

import pl.edu.agh.urban.model.UrbanCell
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId


final case class UrbanConfig(worldType: WorldType,
                             iterationsNumber: Long,

                             signalSuppressionFactor: Double,
                             signalAttenuationFactor: Double,
                             signalSpeedRatio: Int,

                             workersRoot: Int,
                             isSupervisor: Boolean,
                             shardingMod: Int,

                             guiType: GuiType,
                             guiCellSize: Int,

                             originalWidth: Int,
                             originalHeight: Int,
                             originalScale: Double,
                             zoomOut: Int,

                             personalSpaceRange: Double,
                             personalSpaceDetection: Double,
                             personSignal: Signal,
                             targetSignal: Signal,
                             timeStep: Double,

                             pathCreation: String,

                             urbanDataRootPath: String,
                             mapImageFilename: String,
                             tileTypesFilename: String,
                             targetsFilename: String,
                             personBehaviorFilename: String,
                             staticSignalDir: String
                            ) extends XinukConfig {
  implicit def config: UrbanConfig = this

  override val worldWidth: Int = originalWidth / zoomOut

  override val worldHeight: Int = originalHeight / zoomOut

  val scale: Double = originalScale * zoomOut

  val markerSpreadSpeed: Int = (personalSpaceRange / scale).ceil.toInt

  val markerTtl: Int = markerSpreadSpeed

  val mapImage: BufferedImage = Serialization.loadMapImage()

  val tileTypes: Seq[TileType] = Serialization.loadTileTypes()

  val targets: Seq[TargetInfo] = Serialization.loadTargets()

  val personBehavior: PersonBehavior = Serialization.loadPersonBehavior()

  val staticSignal: Map[String, Map[GridCellId, SignalMap]] = Serialization.loadStaticSignal()

  val colorToTileType: Map[Color, TileType] = tileTypes.map { tileType => (tileType.color, tileType) }.toMap

  val idToTileType: Map[TileTypeId, TileType] = tileTypes.map { tileType => (tileType.id, tileType) }.toMap

  val cellToTargetEntrance: Map[GridCellId, TargetInfo] =
  targets.flatMap { building => building.entrances.map { entrance => (entrance.gridId, building) } }.toMap

  val targetTypeToTargets: Map[TargetType, Seq[TargetInfo]] =
  TargetType.values.map { targetType => (targetType, targets.filter { _.targetTypes.contains(targetType) } ) }.toMap

}

object UrbanConfig {
  private val cellToColorSignal: PartialFunction[CellState, Color] = {
    case cellState =>
      val hue = cellState.contents match {
        case Obstacle => 0.66f
        case _: UrbanCell => 0.0f
        case _ => 0.33f
      }
      val mag: Signal = cellState
        .signalMap
        .toSeq
        .map(_._2)
        .sorted(Ordering[Signal].reverse)
        .head
      Color.getHSBColor(hue, 1.0f, Math.pow(mag.value.toFloat, 0.01).toFloat)
  }

  private val cellToColorType: PartialFunction[CellState, Color] = {
    case cellState =>
      cellState.contents match {
        case Obstacle => Color.BLUE
        case urban: UrbanCell => urban.occupant match {
          case None => urban.tileType.color
          case _ => Color.BLACK
        }
        case _ => Color.WHITE
      }
  }

  def cellToColor: PartialFunction[CellState, Color] = cellToColorType
}

case class Coordinates(x: Int, y: Int) {
  def gridId(implicit config: UrbanConfig): GridCellId = GridCellId(x / config.zoomOut, y / config.zoomOut)
}

case class TileType(id: TileTypeId, name: String, color: Color, walkingFactor: Double)

case class TargetInfo(id: String, targetTypes: Set[TargetType], population: Map[String, Int], center: Coordinates, entrances: Seq[Coordinates])

case class PersonBehavior(routine: Map[TimeOfDay, HumanSpawnProfile])

case class HumanSpawnProfile(beginning: LocalTime, end: LocalTime, departurePercent: Double, returnPercent: Double, targets: Map[TargetType, Double])
