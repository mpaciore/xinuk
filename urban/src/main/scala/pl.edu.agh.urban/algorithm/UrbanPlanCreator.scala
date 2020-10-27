package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.config.{Serialization, UrbanConfig}
import pl.edu.agh.urban.model.{Person, PersonMarker, UrbanCell}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId

final case class UrbanPlanCreator() extends PlanCreator[UrbanConfig] {

  private val noop: (Plans, UrbanMetrics) = (Plans.empty, UrbanMetrics.empty)

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: UrbanConfig): (Plans, UrbanMetrics) = {
    if (config.pathCreation != "none") {
      noop
    } else {
      move(iteration, cellId, cellState.contents.asInstanceOf[UrbanCell], neighbourContents)
    }
  }

  private def move(iteration: Long, cellId: CellId, contents: UrbanCell, neighbourContents: Map[Direction, CellContents]): (Plans, UrbanMetrics) = {
    noop
//    val results: Seq[(Map[Direction, Seq[Plan]], UrbanMetrics)] = Seq(
//      contents.occupant.map(movePerson).getOrElse(noop),
//      contents.markers.map(moveMarker),
//      //if (true /*current cell is an entrance*/) handleEntrance(cellId) else noop
//    )
//
//    results.reduce { case (a, b) => mergeResults(a, b)}
//    // TODO: if human personal area token:  spread
//    // TODO: if human:                      move and spawn personal area token or despawn if at destination
//    // TODO: if entrance:                   spawn new human
  }

  private def movePerson(person: Person): (Map[Direction, Seq[Plan]], UrbanMetrics) = ???

  private def moveMarker(marker: PersonMarker): (Map[Direction, Seq[Plan]], UrbanMetrics) = ???

  private def handleEntrance(cellId: CellId): (Map[Direction, Seq[Plan]], UrbanMetrics) = ???

  private def mergeResults(first: (Map[Direction, Seq[Plan]], UrbanMetrics), second: (Map[Direction, Seq[Plan]], UrbanMetrics)): (Map[Direction, Seq[Plan]], UrbanMetrics) = {
    val (firstPlans, firstMetrics) = first
    val (secondPlans, secondMetrics) = second

    val plans = ???
    val metrics = firstMetrics + secondMetrics
    (plans, metrics)
  }

  //  def randomMove(neighbours: Map[Direction, CellContents])(implicit config: UrbanConfig): Map[Direction, Seq[Plan]] = {
  //    val availableDirections = neighbours.filter {
  //      case (_, Empty) => true
  //      case _ => false
  //    }
  //
  //    if (availableDirections.isEmpty) {
  //      Map.empty
  //    } else {
  //
  //      val direction = availableDirections.keys.toSeq(Random.nextInt(availableDirections.size))
  //
  //      val action = StateUpdate(CellState(UrbanTile, SignalMap.empty))
  //      val consequence = StateUpdate(CellState(Empty, SignalMap.empty))
  //
  //      Map((direction, Seq(Plan(action, consequence))))
  //    }
  //  }

  override def initialize(worldShard: WorldShard)(implicit config: UrbanConfig): Unit = {
    // TODO initialize entrances
  }

  override def finalize(worldShard: WorldShard)(implicit config: UrbanConfig): Unit = {
    if (config.pathCreation != "none") {
      val signal: Map[GridCellId, SignalMap] = worldShard
        .localCellIds
        .map(worldShard.cells(_))
        .map(cell => (cell.id.asInstanceOf[GridCellId], cell.state.signalMap))
        .toMap
      Serialization.dumpStaticSignal(signal, config.pathCreation, worldShard.workerId)
    }
  }
}