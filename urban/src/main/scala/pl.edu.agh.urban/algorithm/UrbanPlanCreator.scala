package pl.edu.agh.urban.algorithm

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.urban.algorithm.UrbanUpdate._
import pl.edu.agh.urban.config.{Serialization, TargetType, UrbanConfig}
import pl.edu.agh.urban.model._
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridDirection}

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
    val timeState = config.getTimeState(iteration)

    var allMoves: Seq[(Plans, UrbanMetrics)] = Seq()

    if (timeState.markerPurgeIteration) {
      allMoves = allMoves :+ checkAndPurgeMarkers(cellId, contents, timeState.markerRound)
    }
    if (timeState.personMovementIteration) {
      allMoves = allMoves ++ contents.occupants.map(person => handlePerson(cellId, person, timeState.time, timeState.markerRound, signalMap, neighbourContents))
      allMoves = allMoves ++ contents.entrances.map(entrance => handleEntrance(cellId, entrance, timeState.time, timeState.markerRound))
    }
    allMoves = allMoves ++ contents.markers.map(marker => handleMarker(marker, timeState.markerRound, neighbourContents))

    allMoves.reduceOption[(Plans, UrbanMetrics)] {
      case ((firstPlans, firstMetrics), (secondPlans, secondMetrics)) =>
        (firstPlans ++ secondPlans, firstMetrics + secondMetrics)
    }.getOrElse(noop)
  }

  private def randomFrom[A](list: Seq[A])(implicit config: UrbanConfig): A = {
    list(config.random.nextInt(list.size))
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
    val spawningPlans: (Plans, UrbanMetrics) = if (entrance.targetTypes.contains(TargetType.Residential)) {
      config.getTimeOfDay(time).map { currentTimeOfDay =>
        val personSpawnProbability = config.getPersonSpawnProbability(currentTimeOfDay, entrance.population)
        if (config.random.nextDouble() <= personSpawnProbability) {
          val targetDistribution = config.personBehavior.spawnRoutine(currentTimeOfDay).targets
          var rand = config.random.nextDouble()
          val targetType = targetDistribution.find { case (_, probability) =>
            rand -= probability / 100d
            rand <= 0
          }.map(_._1).get

          chooseTarget(cellId, targetType).map { case (target, travelMode) =>
            val person = travelMode match {
              case TravelMode.Travel =>
                Person.travelling(entrance.targetId, target)
              case TravelMode.Wander =>
                Person.wandering(entrance.targetId, target, time)
              case _ =>
                throw new RuntimeException("Attempted to generate returning person")
            }

            (Plans(None -> Plan(CreatePerson(person, markerRound))), UrbanMetrics.empty)
          }.getOrElse(noop)
        } else {
          noop
        }
      }.getOrElse(noop)
    } else {
      noop
    }

    val returningPlans = entrance.visitors.map {
      case Visitor(person, returnTime) if time >= returnTime =>
        val returningPerson = person.copy(target = person.source, travelMode = TravelMode.Return)
        (Plans(None -> Plan(CreatePerson(returningPerson, markerRound), RemoveVisitor(entrance.targetId, person.id))), UrbanMetrics.empty)
      case _ =>
        noop
    }

    (returningPlans :+ spawningPlans).reduceOption[(Plans, UrbanMetrics)] {
      case ((firstPlans, firstMetrics), (secondPlans, secondMetrics)) =>
        (firstPlans ++ secondPlans, firstMetrics + secondMetrics)
    }.getOrElse(noop)
  }

  private def signalToPercents(signalMap: SignalMap): Map[Direction, Double] = {
    val signalTotal = signalMap.map(_._2).reduceOption(_ + _).map(_.value).getOrElse(0d)
    signalMap.map { case (d, s) => (d, if (signalTotal > 0) s.value / signalTotal else 0d) }
  }

  private def chooseDirection(optimalDirection: Direction, dynamicSignal: SignalMap, allowedDirections: Set[Direction]): Option[Direction] = {
    val optimalDirectionValue = 1d
    val suboptimalDirectionsValueFactor = 0.8d

    val dynamicSignalPercentages = signalToPercents(dynamicSignal)
    val directionScoring = allowedDirections.map {direction =>
      val stepsFromOptimal = direction.asInstanceOf[GridDirection].stepsFrom(optimalDirection.asInstanceOf[GridDirection])
      val initialScore = optimalDirectionValue * math.pow(suboptimalDirectionsValueFactor, stepsFromOptimal)
      (direction, initialScore - dynamicSignalPercentages.getOrElse(direction, 0d) / 2)
    }

    if (directionScoring.isEmpty) {
      None
    } else {
      Some(directionScoring.maxBy(_._2)._1)
    }
  }

  private def handlePerson(cellId: GridCellId, person: Person, time: Double, markerRound: Long, signalMap: SignalMap, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    val (updatedPerson, metrics) = person.travelMode match {
      case TravelMode.Wander =>
        if (time < person.wanderingSegmentEndTime) {
          // stay with current wandering target
          (person, UrbanMetrics.empty)
        } else {
          if (person.wanderingSegmentsRemaining > 0) {
            // change wandering target
            (person.copy(
              target = randomFrom(config.targets).id,
              wanderingSegmentEndTime = time + config.randomSegmentDuration(),
              wanderingSegmentsRemaining = person.wanderingSegmentsRemaining - 1
            ), UrbanMetrics.empty)
          } else {
            // return to source
            (person.copy(target = person.source, travelMode = TravelMode.Return), UrbanMetrics.wanderEnd + UrbanMetrics.returnBeginning)
          }
        }
      case _ =>
        (person, UrbanMetrics.empty)
    }

    val allowedDirections = neighbourContents
      .filter(_._2.asInstanceOf[UrbanCell].isWalkable)
      .keySet
    val staticDirection = config.staticPaths(updatedPerson.target).get(cellId)

    val plans = staticDirection.flatMap { staticDirection =>
      chooseDirection(staticDirection, signalMap, allowedDirections)
    }.map { chosenDirection =>
      Plans(Some(chosenDirection) -> Plan(
        AddPerson(updatedPerson, markerRound),
        RemovePerson(updatedPerson.id),
        KeepPerson(updatedPerson, markerRound)
      ))
    }.getOrElse {
      logger.warn(s"No direction available from $cellId to ${updatedPerson.target}")
      Plans(None -> Plan(KeepPerson(updatedPerson, markerRound)))
    }

    (plans, metrics)
  }

  private def checkAndPurgeMarkers(cellId: GridCellId, contents: UrbanCell, markerRound: Long): (Plans, UrbanMetrics) = {
    val plans = Plans(None -> Plan(PurgeMarkers(markerRound)))
    val metrics = contents.occupants.map {
      person => if (contents.markers.exists(_.personId != person.id)) {
        UrbanMetrics.violation(cellId)
      } else {
        UrbanMetrics.empty
      }
    }.reduceOption(_ + _).getOrElse(UrbanMetrics.empty)

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
