package pl.edu.agh.formin

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{FlatSpecLike, Matchers}
import pl.edu.agh.formin.SchedulerActor.{GetState, StartSimulation}

class SchedulerActorTest extends TestKit(ActorSystem("SchedulerActorTest")) with FlatSpecLike with Matchers with ImplicitSender {

  "A SchedulerActor" should "start stopped" in {
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(self)))
    scheduler ! GetState
    expectMsg(SchedulerActor.State.Stopped(Map.empty))
  }

  it should "fail to start with no workers" in {
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector()))
    watch(scheduler)
    println(expectTerminated(scheduler))
  }

  it should "start simulation and inform workers" in {
    val worker1 = TestProbe("worker1")
    val worker2 = TestProbe("worker2")
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(worker1.ref, worker2.ref)))
    scheduler ! StartSimulation(1)
    scheduler ! GetState
    expectMsgClass(classOf[SchedulerActor.State.Running])
    worker1.expectMsg(WorkerActor.StartIteration(0))
    worker2.expectMsg(WorkerActor.StartIteration(0))
  }

}
