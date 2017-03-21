package pl.edu.agh.formin

import akka.actor.Actor

class WorkerActor(id: WorkerId) extends Actor {
  override def receive: Receive = ???
}

object WorkerActor {

  case class StartIteration(i: Long) extends AnyVal

}


case class WorkerId(value: Int) extends AnyVal