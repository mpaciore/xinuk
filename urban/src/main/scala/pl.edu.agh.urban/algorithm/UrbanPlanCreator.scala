package pl.edu.agh.urban.algorithm

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.urban.algorithm.UrbanUpdate._
import pl.edu.agh.urban.config.{Serialization, TargetType, UrbanConfig}
import pl.edu.agh.urban.model._
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId

import scala.util.Random

final case class UrbanPlanCreator() extends PlanCreator[UrbanConfig] with LazyLogging {

  private val noop: (Plans, UrbanMetrics) = (Plans.empty, UrbanMetrics.empty)

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (config.pathCreation != "None") {
      noop
    } else {
      move(iteration, cellId.asInstanceOf[GridCellId], cellState.signalMap, cellState.contents.asInstanceOf[UrbanCell], neighbourContents)
    }
  }

  private def move(iteration: Long, cellId: GridCellId, signalMap: SignalMap, contents: UrbanCell, neighbourContents: Map[Direction, CellContents])
                  (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val markerRound: Long = (iteration - 1) / config.markerSpreadSpeed // with each full spread range of markers time advances
    val time: Double = config.startTime + markerRound * config.timeStep // time advance step from config
    val personMovementIteration: Boolean = (iteration - 1) % config.markerSpreadSpeed == 0 // person moves at first iteration and once every marker round
    val markerPurgeIteration: Boolean = (iteration - 2) % config.markerSpreadSpeed == 0 // old markers are purged one iteration after person movement step

    var allMoves: Seq[(Plans, UrbanMetrics)] = Seq()

    if (markerPurgeIteration) {
      allMoves = allMoves :+ checkAndPurgeMarkers(cellId, contents, markerRound)
    }
    if (personMovementIteration) {
      allMoves = allMoves :+ contents.occupant.map(person => handlePerson(cellId, person, markerRound, signalMap, neighbourContents)).getOrElse(noop)
      allMoves = allMoves :+ contents.entrance.map(entrance => handleEntrance(cellId, entrance, time, markerRound)).getOrElse(noop)
    }
    allMoves = allMoves ++ contents.markers.map(marker => handleMarker(marker, markerRound, neighbourContents))

    allMoves.reduceOption[(Plans, UrbanMetrics)] {
      case ((firstPlans, firstMetrics), (secondPlans, secondMetrics)) =>
        (firstPlans ++ secondPlans, firstMetrics + secondMetrics)
    }.getOrElse((Plans.empty, UrbanMetrics.empty))
  }

  private def randomFrom[A](list: Seq[A]): A = {
    list(Random.nextInt(list.size))
  }

  private def randomTargetByType(targetType: TargetType)(implicit config: UrbanConfig): Option[(String, TravelMode)] = {
    Some(randomFrom(config.targetTypeToTargets(targetType)).id, TravelMode.Travel)
  }

  private def nearestTargetByType(cellId: GridCellId, targetType: TargetType)
                           (implicit config: UrbanConfig): Option[(String, TravelMode)] = {
    val targetInfo = config.targetTypeToTargets(targetType)
      .minBy(targetInfo => GridCellId.distance(cellId, targetInfo.center.gridId))
    Some(targetInfo.id, TravelMode.Travel)
  }

  private def chooseTarget(cellId: GridCellId, targetType: TargetType)
                          (implicit config: UrbanConfig): Option[(String, TravelMode)] = {
    targetType match {
      case TargetType.Parking =>
        nearestTargetByType(cellId, targetType)
      case TargetType.Bike =>
        None // bikers are not treated as pedestrians
      case TargetType.Bus =>
        randomTargetByType(targetType)
      case TargetType.Outside =>
        randomTargetByType(targetType)
      case TargetType.Playground =>
        nearestTargetByType(cellId, targetType)
      case TargetType.Wander =>
        Some(randomFrom(config.targets).id, TravelMode.Wander)
      case TargetType.Residential =>
        randomTargetByType(targetType)
      case TargetType.Service =>
        randomTargetByType(targetType)
      case TargetType.Social =>
        randomTargetByType(targetType)
      case _ =>
        None
    }
  }

  private def handleEntrance(cellId: GridCellId, entrance: Entrance, time: Double, markerRound: Long)
                            (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (entrance.targetTypes.contains(TargetType.Residential)) {
      config.getTimeOfDay(time).map { currentTimeOfDay =>
        val personSpawnProbability = config.getPersonSpawnProbability(currentTimeOfDay, entrance.population)
        if (Random.nextDouble() <= personSpawnProbability) {
          val targetDistribution = config.personBehavior.routine(currentTimeOfDay).targets
          var rand = Random.nextDouble()
          val targetType = targetDistribution.find { case (_, probability) =>
            rand -= probability / 100d
            rand <= 0
          }.map(_._1).get

          chooseTarget(cellId, targetType).map { case (target, travelMode) =>
            val person = travelMode match {
              case TravelMode.Travel =>
                Person.travelling(entrance.id, target)
              case TravelMode.Wander =>
                Person.wandering(entrance.id, target)
            }

              (Plans(None -> Plan(CreatePerson(person, markerRound))), UrbanMetrics.empty)
          }.getOrElse(noop)
        } else {
          noop
        }
      }.getOrElse(noop)
    } else {
      noop // TODO returning logic
    }
  }

  private def signalToPercents(signalMap: SignalMap): Map[Direction, Double] = {
    val signalTotal = signalMap.map(_._2).reduceOption(_ + _).map(_.value).getOrElse(0d)
    signalMap.map { case (d, s) => (d, if (signalTotal > 0) s.value / signalTotal else 0d) }
  }

  private def chooseDirection(optimalDirection: Direction, dynamicSignal: SignalMap, allowedDirections: Set[Direction]): Option[Direction] = {
    val optimalDirectionValue = 1d
    val suboptimalDirectionsValueFactor = 0.8d

    val directionInitialScoring: Map[Direction, Double] = optimalDirection.withAdjacent
      .map(direction => direction -> optimalDirectionValue * suboptimalDirectionsValueFactor)
      .toMap ++ Map(optimalDirection -> optimalDirectionValue)

    val dynamicSignalPercentages = signalToPercents(dynamicSignal)
    val directionScoring = directionInitialScoring.filter { case (direction, _) => allowedDirections.contains(direction) }
      .map { case (direction, value) => (direction, value - dynamicSignalPercentages.getOrElse(direction, 0d) / 2) }

    if (directionScoring.isEmpty) {
      None
    } else {
      Some(directionScoring.maxBy(_._2)._1)
    }
  }

  private def handlePerson(cellId: GridCellId, person: Person, markerRound: Long, signalMap: SignalMap, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val updatedPerson: Person = person.travelMode match {
      case TravelMode.Travel =>
        person
      case TravelMode.Wander =>
        if (person.wanderingSegmentTimeRemaining > 0) {
          // update wandering duration
          person.copy(wanderingSegmentTimeRemaining = person.wanderingSegmentTimeRemaining - config.timeStep)
        } else {
          if (person.wanderingSegmentsRemaining > 0) {
            // change target
            person.copy(
              target = randomFrom(config.targets).id,
              wanderingSegmentTimeRemaining = config.randomSegmentDuration(),
              wanderingSegmentsRemaining = person.wanderingSegmentsRemaining - 1
            )
          } else {
            // return to source
            person.copy(target = person.source, travelMode = TravelMode.Travel)
          }
        }
    }

    val allowedDirections = neighbourContents.filter(_._2.asInstanceOf[UrbanCell].isWalkable).keySet
    val staticDirection = config.staticPaths(updatedPerson.target).get(cellId)

    staticDirection.map { staticDirection =>
      chooseDirection(staticDirection, signalMap, allowedDirections)
    }.map { chosenDirection =>
      (Plans(chosenDirection -> Plan(
        AddPerson(updatedPerson, markerRound),
        RemovePerson(updatedPerson.id),
        KeepPerson(updatedPerson, markerRound)
      )), UrbanMetrics.empty)
    }.getOrElse {
      logger.warn(s"No direction available from $cellId to ${updatedPerson.target}")
      (Plans(None -> Plan(KeepPerson(updatedPerson, markerRound))), UrbanMetrics.empty)
    }

  }

  private def checkAndPurgeMarkers(cellId: GridCellId, contents: UrbanCell, markerRound: Long): (Plans, UrbanMetrics) = {
    val plans = Plans(None -> Plan(PurgeMarkers(markerRound)))
    val metrics = contents.occupant.map {
      person => if (contents.markers.exists(_.personId != person.id)) {
        UrbanMetrics.violation(cellId)
      } else {
        UrbanMetrics.empty
      }
    }.getOrElse(UrbanMetrics.empty)

    (plans, metrics)
  }

  private def handleMarker(marker: PersonMarker, markerRound: Long, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (marker.round >= markerRound && marker.distance < config.markerDetectionDistance) {
      val spreadPlans = neighbourContents.keys.map(Some(_) -> Plan(AddMarker(marker.spread()))).toSeq
      (Plans(spreadPlans), UrbanMetrics.empty)
    } else {
      noop
    }
  }

  private def bestDirection(signalMap: SignalMap, neighbourContents: Map[Direction, CellContents], neighbourContentsFilter: CellContents => Boolean): Direction = {
    signalMap.filter {
      case (direction, _) => neighbourContents.contains(direction) && neighbourContentsFilter(neighbourContents(direction))
    }.toSeq.maxBy(_._2)._1
  }

  override def finalize(worldShard: WorldShard)(implicit config: UrbanConfig): Unit = {
    if (config.pathCreation != "None") {
      val signal: Map[GridCellId, SignalMap] = worldShard
        .localCellIds
        .map(worldShard.cells(_))
        .map(cell => (cell.id.asInstanceOf[GridCellId], cell.state.signalMap))
        .toMap
      Serialization.dumpStaticSignal(signal, config.pathCreation, worldShard.workerId)
    }
  }
}