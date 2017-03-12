package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Terminated}

object Greeter {

  case object Greet

  case object Done

}

class Greeter extends Actor {
  def receive: Receive = {
    case Greeter.Greet =>
      println("Hello World!")
      sender() ! Greeter.Done
  }
}

class Terminator(ref: ActorRef) extends Actor with ActorLogging {
  context watch ref

  def receive: Receive = {
    case Terminated(_) =>
      log.info("{} has terminated, shutting down system", ref.path)
      context.system.terminate()
  }
}

class HelloWorld extends Actor {

  override def preStart(): Unit = {
    // create the greeter actor
    val greeter = context.actorOf(Props[Greeter], "greeter")
    // tell it to perform the greeting
    greeter ! Greeter.Greet
  }

  def receive: Receive = {
    // when the greeter is done, stop this actor and with it the application
    case Greeter.Done => context.stop(self)
  }
}

object HelloWorld extends App {
  val system = ActorSystem("formin")
  val greeter = system.actorOf(Props[HelloWorld], "helloworld")
  system.actorOf(Props(classOf[Terminator], greeter))
}
