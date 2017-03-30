package pl.edu.agh.formin

import akka.actor.{Actor, Props}
import pl.edu.agh.formin.SchedulerActor.IterationPartFinished
import pl.edu.agh.formin.WorkerActor.StartIteration
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.{EmptyCell, Grid}

import scala.util.Random

class WorkerActor private(id: WorkerId)(implicit config: ForminConfig) extends Actor {

  private var grid = Grid.empty

  private val random = new Random(System.nanoTime())

  override def receive: Receive = stopped

  private def propagateSignal(): Unit = {
    (0 until config.signalSpeedRatio).foreach { _ =>
      val cells = Array.tabulate(config.gridSize, config.gridSize)((x, y) =>
        grid.propagatedSignal(x, y)
      )
      grid = Grid(cells)
    }
  }

  def stopped: Receive = {
    case StartIteration(1) =>
      val empty = EmptyCell()
      for {
        x <- 0 until config.gridSize
        y <- 0 until config.gridSize
        if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
      } {
        if (random.nextDouble() < config.spawnChance) {
          grid.cells(x)(y) =
            if (random.nextDouble() < config.foraminiferaSpawnChance) empty.withForaminifera(config.foraminiferaStartEnergy)
            else empty.withAlgae
        }
      }
      propagateSignal()
      sender() ! IterationPartFinished(1, SimulationStatus(id, grid))
      context.become(started)
  }

  def started: Receive = {
    case StartIteration(i) =>
      propagateSignal()
      sender() ! IterationPartFinished(i, SimulationStatus(id, grid))
  }
}

object WorkerActor {

  case class StartIteration(i: Long) extends AnyVal

  def props(id: WorkerId)(implicit config: ForminConfig): Props = {
    Props(new WorkerActor(id))
  }
}


case class WorkerId(value: Int) extends AnyVal