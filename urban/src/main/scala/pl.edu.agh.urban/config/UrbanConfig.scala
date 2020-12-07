package pl.edu.agh.urban.config

import java.awt.Color
import java.awt.image.BufferedImage
import java.time.LocalTime

import pl.edu.agh.urban.model.UrbanCell
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId

import scala.util.Random


final case class UrbanConfig(
                              worldType: WorldType,
                              iterationsNumber: Long,
                              iterationFinishedLogFrequency: Long,
                              skipEmptyLogs: Boolean,

                              signalSuppressionFactor: Double,
                              signalAttenuationFactor: Double,
                              signalDisabled: Boolean,

                              workersX: Int,
                              workersY: Int,
                              isSupervisor: Boolean,
                              shardingMod: Int,

                              guiType: GuiType,
                              guiCellSize: Int,
                              guiStartIteration: Long,
                              guiUpdateFrequency: Long,

                              originalWidth: Int,
                              originalHeight: Int,
                              originalScale: Double,
                              zoomOut: Int,

                              timeStep: Double,
                              startTime: Double,

                              personalSpaceDetection: Double,
                              closeViolationThreshold: Double,
                              markerMaxDistance: Double,
                              markerMaxAge: Int,
                              markerSpreadSpeed: Int,
                              avoidMarkers: Boolean,
                              personMemorySize: Int,

                              personSignal: Signal,
                              targetSignal: Signal,

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

  val mapImage: BufferedImage = Serialization.loadMapImage()

  val tileTypes: Seq[TileType] = Serialization.loadTileTypes()

  val targets: Seq[TargetInfo] = Serialization.loadTargets()

  val personBehavior: PersonBehavior = Serialization.loadPersonBehavior()

  val staticPaths: Map[String, Map[GridCellId, Direction]] = Serialization.loadStaticPaths()

  val colorToTileType: Map[Color, TileType] = tileTypes.map { tileType => (tileType.color, tileType) }.toMap

  val targetTypeToTargets: Map[TargetType, Seq[TargetInfo]] =
    TargetType.values.map { targetType => (targetType, targets.filter(_.targetTypes.contains(targetType))) }.toMap

  def getTimeState(iteration: Long): TimeState = {
    // with each full spread range of markers time advances
    val markerRound: Long = (iteration - 1) / markerSpreadSpeed
    // time advance step from config
    val time: Double = config.startTime + markerRound * timeStep
    // person moves at first iteration and once every marker round
    val personMovementIteration: Boolean = (iteration - 1) % markerSpreadSpeed == 0
    // old markers are purged one iteration after person movement step
    val markerPurgeIteration: Boolean = (iteration - 2) % markerSpreadSpeed == 0
    TimeState(markerRound, time, personMovementIteration, markerPurgeIteration)
  }

  def getTimeOfDay(time: Double): Option[TimeOfDay] = {
    val currentTime = LocalTime.ofSecondOfDay(time.toLong % 86400L)
    personBehavior.spawnRoutine.find {
      case (_, spawnProfile) => currentTime.isAfter(spawnProfile.beginning) && currentTime.isBefore(spawnProfile.end)
    }.map(_._1)
  }

  def getPersonSpawnProbability(timeOfDay: TimeOfDay, population: Long): Double = {
    val percent = personBehavior.spawnRoutine(timeOfDay).departurePercent // percent of population departing each 15 minutes
    (population * percent / 100d) / (900d / timeStep)
  }

  def randomSegmentDuration(): Double = wanderSegmentDurationMean + random.nextGaussian() * wanderSegmentDurationStd

  def randomSegments(): Long = wanderSegmentsMean + random.nextLong(wanderSegmentsSpread * 2 + 1) - wanderSegmentsSpread

  def randomVisitTime(targetType: TargetType): Double = {
    val durationDistribution = personBehavior.visitingRoutine(targetType)
    durationDistribution.mean + random.nextGaussian() * durationDistribution.std
  }
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
        case urban: UrbanCell => urban.occupants.size match {
          case 0 => urban.tileType.color
          case _ => Color.BLACK
        }
        case _ => Color.WHITE
      }
  }

  def cellToColor: PartialFunction[CellState, Color] = cellToColorType
}

case class TimeState(markerRound: Long, time: Double, personMovementIteration: Boolean, markerPurgeIteration: Boolean)

case class Coordinates(x: Int, y: Int) {
  def gridId(implicit config: UrbanConfig): GridCellId = GridCellId(x / config.zoomOut, y / config.zoomOut)
}

case class TileType(id: TileTypeId,
                    name: String,
                    color: Color,
                    walkingFactor: Double)

case class TargetInfo(id: String,
                      targetTypes: Set[TargetType],
                      population: Map[String, Int],
                      center: Coordinates,
                      entrances: Seq[Coordinates])

case class PersonBehavior(spawnRoutine: Map[TimeOfDay, HumanSpawnProfile],
                          visitingRoutine: Map[TargetType, DurationDistribution],
                          restrictionFactors: Map[TargetType, Double])

case class HumanSpawnProfile(beginning: LocalTime,
                             end: LocalTime,
                             departurePercent: Double,
                             returnPercent: Double,
                             targets: Map[TargetType, Double])

case class DurationDistribution(mean: Double,
                                std: Double)
