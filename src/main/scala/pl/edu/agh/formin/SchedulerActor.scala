package pl.edu.agh.formin

import java.{lang => jl}

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.avsystem.commons._
import com.google.common.cache.CacheBuilder
import pl.edu.agh.formin.SchedulerActor._
import pl.edu.agh.formin.gui.GuiActor.NewIteration
import pl.edu.agh.formin.model.Grid

import scala.collection.mutable

class SchedulerActor(workers: Vector[ActorRef]) extends Actor with ActorLogging {
  require(workers.nonEmpty, "Workers cannot be empty")

  //todo: this could benefit from being totally unsynchronized, but was a fast way to get a bounded map
  private val iteration2status: mutable.Map[jl.Long, IterationStatus] =
    CacheBuilder.newBuilder()
      .maximumSize(100)
      .concurrencyLevel(1)
      .build[jl.Long, IterationStatus]().asMap().asScala

  private var iterations: Long = _

  private val registered: mutable.Set[ActorRef] = mutable.Set.empty

  override def receive: Receive = stopped

  private def status: Map[Long, IterationStatus] = {
    iteration2status.map { case (iteration, status) => (iteration: Long, status) }(scala.collection.breakOut)
  }

  private def startIteration(n: Long): Unit = {
    iteration2status.update(n, IterationStatus.empty())
    workers.foreach(_ ! WorkerActor.StartIteration(n))
  }

  def stopped: Receive = {
    case StartSimulation(iterations) =>
      this.iterations = iterations
      if (iterations <= 0) {
        context.become(finished)
      } else {
        log.info("Simulation started, iterations={}", iterations)
        startIteration(1)
        context.become(started)
      }
    case GetState =>
      sender() ! State.Stopped
    case Register =>
      registered += sender()
  }

  def started: Receive = {
    case IterationPartFinished(iteration, status) =>
      iteration2status.getOpt(iteration - 1).foreach(_.remove(status.worker))
      iteration2status.getOpt(iteration) match {
        case Opt(currentIterationStatus) =>
          currentIterationStatus.add(status)
          if (currentIterationStatus.size == workers.size) {
            notifyListeners(iteration)
            //todo if headless
            //self ! IterationFinished(iteration)
          }
        case Opt.Empty =>
          log.warning("Cache miss on iteration {} part finish for worker {}", iteration, status.worker)
      }
    case Register =>
      registered += sender()
    case IterationFinished(i) if i == iterations =>
      self ! StopSimulation
    case IterationFinished(i) =>
      startIteration(i + 1)
    case GetState =>
      sender() ! State.Running(status)
    case StopSimulation =>
      log.info("Simulation stopped.")
      context.become(finished)
  }

  private def notifyListeners(iteration: Long): Unit = {
    val finishedIterationStatus = status(iteration)
    registered.foreach(_ ! NewIteration(finishedIterationStatus, iteration))
  }

  def finished: Receive = {
    case GetState =>
      sender() ! State.Finished(status)
  }
}

object SchedulerActor {

  case class StartSimulation(iterations: Long) extends AnyVal

  case object Register

  case object GetState

  case class IterationPartFinished(iteration: Long, simulationStatus: SimulationStatus)

  case class IterationFinished(i: Long) extends AnyVal

  case object StopSimulation

  sealed trait State

  object State {

    case object Stopped extends State

    case class Running(status: Map[Long, IterationStatus]) extends State

    case class Finished(status: Map[Long, IterationStatus]) extends State

  }

}

case class IterationStatus private() {
  def getGridForWorker(id: WorkerId): Option[Grid] = {
    worker2grid.get(id)
  }

  private val worker2grid = mutable.HashMap[WorkerId, Grid]()

  def add(status: SimulationStatus): Unit = {
    worker2grid += status.worker -> status.grid
  }

  def remove(id: WorkerId): Unit = {
    worker2grid.remove(id)
  }

  def size: Int = {
    worker2grid.size
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[IterationStatus]

  override def equals(other: Any): Boolean = other match {
    case that: IterationStatus =>
      (that canEqual this) &&
        worker2grid == that.worker2grid
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(worker2grid)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object IterationStatus {
  def empty(): IterationStatus = IterationStatus()
}

case class SimulationStatus(worker: WorkerId, grid: Grid)