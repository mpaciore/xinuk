package pl.edu.agh.urban.config

import java.awt.Color
import java.awt.image.BufferedImage
import java.time.LocalTime

import pl.edu.agh.urban.model.UrbanCell
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId

import scala.util.Random


final case class UrbanConfig(worldType: WorldType,
                             iterationsNumber: Long,
                             iterationFinishedLogFrequency: Long,

                             signalSuppressionFactor: Double,
                             signalAttenuationFactor: Double,
                             signalSpeedRatio: Int,

                             workersRoot: Int,
                             isSupervisor: Boolean,
                             shardingMod: Int,

                             guiType: GuiType,
                             guiCellSize: Int,
                             guiUpdateFrequency: Long,

                             originalWidth: Int,
                             originalHeight: Int,
                             originalScale: Double,
                             zoomOut: Int,

                             personalSpaceDetection: Double,
                             personSignal: Signal,
                             targetSignal: Signal,
                             timeStep: Double,
                             startTime: Double,

                             wanderSegmentsMean: Long,
                             wanderSegmentsSpread: Long,
                             wanderSegmentDurationMean: Double,
                             wanderSegmentDurationStd: Double,

                             pathCreation: String,

                             urbanDataRootPath: String,
                             mapImageFilename: String,
                             tileTypesFilename: String,
                             targetsFilename: String,
                             personBehaviorFilename: String,
                             staticSignalDir: String,
                             staticPathsDir: String
                            ) extends XinukConfig {
  implicit def config: UrbanConfig = this

  val random: Random = new Random()

  override val worldWidth: Int = originalWidth / zoomOut

  override val worldHeight: Int = originalHeight / zoomOut

  val scale: Double = originalScale * zoomOut

  val markerDetectionDistance: Int = (personalSpaceDetection / scale).ceil.toInt

  val markerSpreadSpeed: Int = markerDetectionDistance

  val mapImage: BufferedImage = Serialization.loadMapImage()

  val tileTypes: Seq[TileType] = Serialization.loadTileTypes()

  val targets: Seq[TargetInfo] = Serialization.loadTargets()

  val personBehavior: PersonBehavior = Serialization.loadPersonBehavior()

  val staticPaths: Map[String, Map[GridCellId, Direction]] = Serialization.loadStaticPaths()

  val colorToTileType: Map[Color, TileType] = tileTypes.map { tileType => (tileType.color, tileType) }.toMap

  val idToTileType: Map[TileTypeId, TileType] = tileTypes.map { tileType => (tileType.id, tileType) }.toMap

  val cellToTargetEntrance: Map[GridCellId, TargetInfo] =
    targets.flatMap { building => building.entrances.map { entrance => (entrance.gridId, building) } }.toMap

  val targetTypeToTargets: Map[TargetType, Seq[TargetInfo]] =
    TargetType.values.map { targetType => (targetType, targets.filter(_.targetTypes.contains(targetType))) }.toMap

  def getTimeOfDay(time: Double): Option[TimeOfDay] = {
    val currentTime = LocalTime.ofSecondOfDay(time.toLong % 86400L)
    personBehavior.routine.find {
      case (_, spawnProfile) => currentTime.isAfter(spawnProfile.beginning) && currentTime.isBefore(spawnProfile.end)
    }.map(_._1)
  }

  def getPersonSpawnProbability(timeOfDay: TimeOfDay, population: Long): Double = {
    val percent = personBehavior.routine(timeOfDay).departurePercent // percent of population departing each 15 minutes
    (population * percent / 100d) / (900d / timeStep)
  }
  def randomSegmentDuration(): Double = wanderSegmentDurationMean + random.nextGaussian() * wanderSegmentDurationStd

  def randomSegments(): Long = wanderSegmentsMean + random.nextLong(wanderSegmentsSpread * 2 + 1) - wanderSegmentsSpread
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
