package pl.edu.agh.formin

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.concurrent.Eventually
import org.scalatest.{FlatSpecLike, Matchers}
import pl.edu.agh.formin.SchedulerActor.{GetState, StartSimulation}
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.{Energy, Grid, Signal}

class SchedulerActorTest
  extends TestKit(ActorSystem("SchedulerActorTest"))
    with FlatSpecLike with Matchers with ImplicitSender with Eventually {

  implicit val config = ForminConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppresionFactor = 0.5,
    gridSize = 10,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1)
  )

  "A SchedulerActor" should "be in stopped state" in {
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(self)))
    scheduler ! GetState
    expectMsg(SchedulerActor.State.Stopped)
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
    worker1.expectMsg(WorkerActor.StartIteration(1))
    worker2.expectMsg(WorkerActor.StartIteration(1))
  }

  it should "only finish iteration when all workers finished" in {
    val worker1 = TestProbe("worker1")
    val worker2 = TestProbe("worker2")
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(worker1.ref, worker2.ref)))
    scheduler ! StartSimulation(2)

    worker1.expectMsg(WorkerActor.StartIteration(1))
    worker1.send(scheduler, SchedulerActor.IterationPartFinished(1, SimulationStatus(WorkerId(1), Grid.empty)))

    scheduler ! GetState
    val afterFirstWorker = expectMsgClass(classOf[SchedulerActor.State.Running])
    afterFirstWorker.status.keySet should contain only 1L

    worker2.expectMsg(WorkerActor.StartIteration(1))
    worker2.send(scheduler, SchedulerActor.IterationPartFinished(1, SimulationStatus(WorkerId(2), Grid.empty)))

    worker1.expectMsg(WorkerActor.StartIteration(2))
    worker2.expectMsg(WorkerActor.StartIteration(2))

    scheduler ! GetState
    val afterSecondWorker = expectMsgClass(classOf[SchedulerActor.State.Running])
    afterSecondWorker.status.keySet should contain only(1L, 2L)
  }

  it should "be in finished state" in {
    val worker = TestProbe("worker1")

    val nonSense = system.actorOf(Props(classOf[SchedulerActor], Vector(worker.ref)))
    nonSense ! StartSimulation(-1)
    nonSense ! GetState
    expectMsg(SchedulerActor.State.Finished(Map.empty))

    val zero = system.actorOf(Props(classOf[SchedulerActor], Vector(worker.ref)))
    zero ! StartSimulation(0)
    zero ! GetState
    expectMsg(SchedulerActor.State.Finished(Map.empty))

    val worker1 = TestProbe("worker1")
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(worker1.ref)))
    scheduler ! StartSimulation(1)

    val iterationStatus = IterationStatus.empty()
    worker1.expectMsg(WorkerActor.StartIteration(1))
    val simulationStatus = SimulationStatus(WorkerId(1), Grid.empty)
    worker1.send(scheduler, SchedulerActor.IterationPartFinished(1, simulationStatus))
    iterationStatus.add(simulationStatus)

    eventually {
      scheduler ! GetState
      expectMsg(SchedulerActor.State.Finished(Map(1L -> iterationStatus)))
    }
  }

  it should "provide iteration state" in {
    val worker1 = TestProbe("worker1")
    val worker2 = TestProbe("worker2")
    val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(worker1.ref, worker2.ref)))
    scheduler ! StartSimulation(1)

    scheduler ! GetState
    val iterationStatus = IterationStatus.empty()
    expectMsg(SchedulerActor.State.Running(Map(1L -> iterationStatus)))

    worker1.expectMsg(WorkerActor.StartIteration(1))
    val simulationStatus = SimulationStatus(WorkerId(1), Grid.empty)
    worker1.send(scheduler, SchedulerActor.IterationPartFinished(1, simulationStatus))
    iterationStatus.add(simulationStatus)

    scheduler ! GetState
    expectMsg(SchedulerActor.State.Running(Map(1L -> iterationStatus)))

    worker2.expectMsg(WorkerActor.StartIteration(1))
    val simulationStatus2 = SimulationStatus(WorkerId(2), Grid.empty)
    worker2.send(scheduler, SchedulerActor.IterationPartFinished(1, simulationStatus2))
    iterationStatus.add(simulationStatus2)

    eventually {
      scheduler ! GetState
      expectMsg(SchedulerActor.State.Finished(Map(1L -> iterationStatus)))
    }
  }

}
