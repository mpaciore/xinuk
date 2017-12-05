package pl.edu.agh.formin

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.scalatest.{FlatSpecLike, Matchers}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model.parallel.{Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.model.{Energy, Signal}

class ParallerTest extends FlatSpecLike with Matchers with Eventually with ScalaFutures {
  implicit val config: ForminConfig = ForminConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppressionFactor = 0.5,
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 3,
    iterationsNumber = 3,
    isSupervisor = true
  )

  trait Fixture {
    implicit val system: ActorSystem = ActorSystem("WorkerActorTest")
  }

  "A WorkerActors" should "have defined buffers correctly" in new Fixture {
    val workerRegion = TestProbe("worker1")
    val worker= TestActorRef(WorkerActor.props(config)) //system.actorOf(WorkerActor.props(config))
    val workerWorker = worker.asInstanceOf[WorkerActor].grid
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5), Vector(Neighbour(NeighbourPosition.Bottom)), workerRegion.ref)
    val msg = workerRegion.expectMsgAnyClassOf(classOf[WorkerActor.IterationPartFinished])
    msg.iteration shouldBe 1
  }

}
