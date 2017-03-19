package pl.edu.agh.formin

import java.{util => ju}

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.avsystem.commons._
import pl.edu.agh.formin.SchedulerActor.{IterationFinished, IterationPartFinished, StartSimulation, StopSimulation}
import pl.edu.agh.formin.model.Grid

import scala.collection.mutable

class SchedulerActor(workers: Vector[ActorRef]) extends Actor with ActorLogging {
  private val iteration2status: mutable.Map[Long, IterationStatus] =
    new ju.TreeMap[Long, IterationStatus].asScala

  private var iterations: Long = _

  override def receive: Receive = stopped

  def stopped: Receive = {
    case StartSimulation(iterations) =>
      this.iterations = iterations
      log.info(s"Simulation started, iterations=$iterations")
      context.become(started)
  }

  def started: Receive = {
    case StopSimulation =>
      log.info("Simulation stopped.")
      context.become(stopped)
    case IterationPartFinished(iteration, status) =>
      iteration2status.getOpt(iteration - 1).foreach(_.remove(status.worker))
      val currentIterationStatus = iteration2status(iteration)
      currentIterationStatus.add(status)
      if (currentIterationStatus.size == workers.size) self ! IterationFinished(iteration)
    case IterationFinished(i) if i == iterations =>
      self ! StopSimulation
    case IterationFinished(i) =>
      val nextIteration = i + 1
      iteration2status.update(nextIteration, IterationStatus.empty())
      workers.foreach(_ ! WorkerActor.StartIteration(nextIteration))
  }
}


object SchedulerActor {

  case class StartSimulation(iterations: Long) extends AnyVal

  case class IterationPartFinished(iteration: Long, simulationStatus: SimulationStatus)

  case class IterationFinished(i: Long) extends AnyVal

  case object StopSimulation

}

case class IterationStatus private() {
  private val worker2grid = new ju.TreeMap[WorkerId, Grid]().asScala

  def add(status: SimulationStatus): Unit = {
    worker2grid += status.worker -> status.grid
  }

  def remove(id: WorkerId): Unit = {
    worker2grid.remove(id)
  }

  def size: Int = {
    worker2grid.size
  }

}

object IterationStatus {
  def empty(): IterationStatus = IterationStatus()
}

case class SimulationStatus(worker: WorkerId, grid: Grid)