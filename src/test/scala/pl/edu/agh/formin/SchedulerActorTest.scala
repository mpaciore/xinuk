package pl.edu.agh.formin

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{FlatSpecLike, Matchers}
import pl.edu.agh.formin.SchedulerActor.GetState


class SchedulerActorTest extends TestKit(ActorSystem("SchedulerActorTest")) with FlatSpecLike with Matchers with ImplicitSender {

  "A SchedulerActor" should "start stopped" in {
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(self)))
    scheduler ! GetState
    expectMsg(SchedulerActor.State.Stopped(Map.empty))
  }

}
