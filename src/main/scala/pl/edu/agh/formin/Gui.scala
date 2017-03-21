package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef}
import pl.edu.agh.formin.SchedulerActor.Register
import pl.edu.agh.formin.IterationStatus
import pl.edu.agh.formin.Gui._

class Gui(scheduler: ActorRef, worker: ActorRef) extends Actor with ActorLogging{

  override def receive: Receive = started

  override def preStart = {
    scheduler ! Register
    log.info("GUI started")
  }

  def started: Receive = {
    case PrintData(state) =>


    case CloseGui =>

  }
}

object Gui {

  case class PrintData(state: IterationStatus) extends AnyVal

  case object CloseGui

  sealed trait State

  object State {

    case object Started extends State

  }

}
