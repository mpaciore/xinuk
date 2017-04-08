package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.avsystem.commons._
import pl.edu.agh.formin.SchedulerActor._
import pl.edu.agh.formin.gui.GuiActor.NewIteration
import pl.edu.agh.formin.model.Grid

import scala.collection.mutable

class SchedulerActor(workers: Vector[ActorRef]) extends Actor with ActorLogging {
  require(workers.nonEmpty, "Workers cannot be empty")

  private val iteration2status: mutable.Map[Long, IterationStatus] = mutable.Map.empty[Long, IterationStatus]

  private var iterations: Long = _

  private val registered: mutable.Set[ActorRef] = mutable.Set.empty

  override def receive: Receive = stopped

  private def startIteration(i: Long): Unit = {
    iteration2status.remove(i - 1)
    iteration2status.update(i, IterationStatus.empty())
    workers.foreach(_ ! WorkerActor.StartIteration(i))
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
      iteration2status.getOpt(iteration) match {
        case Opt(currentIterationStatus) =>
          currentIterationStatus.add(status)
          if (currentIterationStatus.size == workers.size) {
            notifyListeners(iteration)
            //todo if headless
            //Thread.sleep(10)
            self ! IterationFinished(iteration)
          }
        case Opt.Empty =>
          log.warning("Cache miss on iteration {} part finish for worker {}", iteration, status.worker)
      }
    case IterationFinished(i) =>
      if (i % 100 == 0) {
        log.info("Iteration finished: {}", i)
      }
      if (i == iterations) self ! StopSimulation
      else startIteration(i + 1)
    case Register =>
      registered += sender()
    case GetState =>
      sender() ! State.Running(iteration2status.toMap)
    case StopSimulation =>
      log.info("Simulation stopped.")
      context.become(finished)
  }

  private def notifyListeners(iteration: Long): Unit = {
    registered.foreach(_ ! NewIteration(iteration2status(iteration), iteration))
  }

  def finished: Receive = {
    case GetState =>
      sender() ! State.Finished(iteration2status.toMap)
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

    case class Running(status: IMap[Long, IterationStatus]) extends State

    case class Finished(status: IMap[Long, IterationStatus]) extends State

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