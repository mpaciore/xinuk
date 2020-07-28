package pl.edu.agh.torch.algorithm

import pl.edu.agh.torch.config.TorchConfig
import pl.edu.agh.torch.model.{Exit, Fire, Person}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, StateUpdate}
import pl.edu.agh.xinuk.model.{CellState, Direction, Obstacle, SignalMap}

import scala.util.Random

case class TorchPlanCreator() extends PlanCreator[TorchConfig] {

  override def createPlans(iteration: Long, cellState: CellState, neighbourStates: Map[Direction, CellState])
                          (implicit config: TorchConfig): (Map[Direction, Seq[Plan]], TorchMetrics) = {
    cellState.contents match {
      case Fire => (spreadFire(iteration, neighbourStates), TorchMetrics.fire)
      case Person(speed) => (movePerson(iteration, speed, cellState.signalMap, neighbourStates), TorchMetrics.person)
      case Exit => (Map.empty, TorchMetrics.exit)
      case _ => (Map.empty, TorchMetrics.empty)
    }
  }

  private def spreadFire(iteration: Long, neighbourStates: Map[Direction, CellState])
                        (implicit config: TorchConfig): Map[Direction, Seq[Plan]] = {
    if (iteration % config.fireSpreadingFrequency == 0) {
      val availableDirections: Seq[Direction] = neighbourStates.filterNot(_._2.contents == Obstacle).keys.toSeq
      val targetDirection = availableDirections(Random.nextInt(availableDirections.size))
      Map((targetDirection, Seq(Plan(StateUpdate(CellState(Fire))))))
    } else {
      Map.empty
    }
  }



  private def movePerson(iteration: Long, speed: Int, cellSignal: SignalMap, neighbourStates: Map[Direction, CellState]): Map[Direction, Seq[Plan]] = {
    if (iteration % speed == 0) {
      Map.empty

//      val destination = selectDestinationCell(calculatePossibleDestinations(x, y))
//
//      destination match {
//        case Opt((i, j, HumanAccessible(destination))) =>
//          newGrid.setCellAt(i, j, destination.withHuman(cell.crowd, cell.speed))
//          newGrid.getCellAt(i, j).cell match {
//            case EscapeCell(_) => peopleEscapes += 1
//            case _ =>
//          }
//        case Opt((i, j, inaccessibleDestination)) =>
//          throw new RuntimeException(s"Human selected inaccessible destination ($i,$j): $inaccessibleDestination")
//        case Opt.Empty =>
//          newGrid.setCellAt(x, y, cell.copy(cell.signal, cell.crowd, cell.speed))
//      }
    } else {
      Map.empty
    }
  }

//  def calculatePossibleDestinations(x: Int, y: Int): Iterator[(Int, Int)] = {
//    val enhancedCell = grid.getCellAt(x, y)
//    random.shuffle(enhancedCell.cell.signal
//      .toList
//      .map(_.swap)
//      .filter { case (_, direction) => enhancedCell.neighbours.contains(direction) })
//      .sortBy(_._1)(Ordering[Signal].reverse)
//      .iterator
//      .map { case (_, direction) => enhancedCell.neighbours(direction) }
//  }
//
//  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int)]): commons.Opt[(Int, Int, GridPart)] = {
//    possibleDestinations
//      .map {
//        case (i, j) => (i, j, grid.getCellAt(i, j).cell, newGrid.getCellAt(i, j).cell)
//      }
//      .collectFirstOpt {
//        case (i, j, currentCell@HumanAccessible(_), HumanAccessible(_)) => (i, j, currentCell)
//      }
//  }
//
//  def stayInPlace(cell: HumanCell, x: Int, y: Int): Unit = {
//    newGrid.setCellAt(x, y, cell.copy(cell.signal, cell.crowd, cell.speed))
//  }
}