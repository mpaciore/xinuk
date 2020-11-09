package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.algorithm.UrbanUpdate._
import pl.edu.agh.urban.config.{Serialization, TargetType, UrbanConfig}
import pl.edu.agh.urban.model.{Entrance, Person, PersonMarker, UrbanCell}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId

import scala.util.Random

final case class UrbanPlanCreator() extends PlanCreator[UrbanConfig] {

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
      allMoves = allMoves :+ contents.entrance.map(entrance => handleEntrance(entrance, time, markerRound)).getOrElse(noop)
    }
    allMoves = allMoves ++ contents.markers.map(marker => handleMarker(marker, markerRound, neighbourContents))

    allMoves.reduceOption[(Plans, UrbanMetrics)] {
      case ((firstPlans, firstMetrics), (secondPlans, secondMetrics)) =>
        (firstPlans ++ secondPlans, firstMetrics + secondMetrics)
    }.getOrElse((Plans.empty, UrbanMetrics.empty))
  }

  private def handleEntrance(entrance: Entrance, time: Double, markerRound: Long)
                            (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (entrance.targetTypes.contains(TargetType.Residential)) {
      val currentTimeOfDay = config.getTimeOfDay(time)
      if (currentTimeOfDay.isDefined) {
        val currentSpawnInterval = config.getHumanSpawnInterval(currentTimeOfDay.get, entrance.population)
        val randomSkew = Random.nextInt(10) - 5
        if (time - entrance.lastDepartureTime + randomSkew > currentSpawnInterval) {
          (Plans(None -> Plan(
            CreatePerson(Person(entrance.id, Some("BB01")), markerRound),
            UpdateEntrance(time)
          )), UrbanMetrics.empty)
        } else {
          noop
        }
      } else {
        noop
      }
    } else {
      noop
    }
  }

  private def signalToPercents(signalMap: SignalMap): Map[Direction, Double] = {
    val signalTotal = signalMap.map(_._2).reduceOption(_ + _).map(_.value).getOrElse(0d)
    signalMap.map { case (d, s) => (d, if (signalTotal > 0) s.value / signalTotal else 0d) }
  }

  private def chooseDirection(optimalDirectionOpt: Option[Direction], dynamicSignal: SignalMap, allowedDirections: Set[Direction]): Option[Direction] = {
    optimalDirectionOpt.map { optimalDirection =>
      val optimalDirectionValue = 1d
      val suboptimalDirectionValue = 0.8d

      val dynamicSignalPercentages = signalToPercents(dynamicSignal)

      val directionScoring: Map[Direction, Double] = optimalDirection.withAdjacent
        .map(direction => direction -> suboptimalDirectionValue)
        .toMap ++ Map(optimalDirection -> optimalDirectionValue)

      directionScoring.filter { case (direction, _) => allowedDirections.contains(direction) }
        .map { case (direction, value) => (direction, value - dynamicSignalPercentages.getOrElse(direction, 0d) / 2) }
        .maxBy(_._2)._1
    }
  }

  private def handlePerson(cellId: GridCellId, person: Person, markerRound: Long, signalMap: SignalMap, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (person.target.isDefined) {
      val allowedDirections = neighbourContents.filter(_._2.asInstanceOf[UrbanCell].isWalkable).keySet
      val staticDirection = config.staticPaths(person.target.get).get(cellId)

      val direction = chooseDirection(staticDirection, signalMap, allowedDirections)

      if (direction.isDefined) {
        (Plans(direction -> Plan(
          AddPerson(person, markerRound),
          RemovePerson(person.id),
          KeepPerson(person.id, markerRound)
        )), UrbanMetrics.empty)
      } else {
        (Plans(None -> Plan(KeepPerson(person.id, markerRound))), UrbanMetrics.empty)
      }
    } else {
      noop
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