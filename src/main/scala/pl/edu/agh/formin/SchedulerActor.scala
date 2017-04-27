package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef}
import pl.edu.agh.formin.SchedulerActor._
import pl.edu.agh.formin.model.Grid

class SchedulerActor(workers: Vector[ActorRef]) extends Actor with ActorLogging {
  require(workers.nonEmpty, "Workers cannot be empty")

  override def receive: Receive = stopped

  private def initializeWorkers(): Unit = {
    workers.foreach(_ ! WorkerActor.NeighboursInitialized(Set.empty))
  }

  def stopped: Receive = {
    case StartSimulation(iterations) =>
        log.info("Simulation started, iterations={}", iterations)
      initializeWorkers()
  }

}

object SchedulerActor {

  case class StartSimulation(iterations: Long) extends AnyVal

}

case class SimulationStatus(worker: WorkerId, grid: Grid)