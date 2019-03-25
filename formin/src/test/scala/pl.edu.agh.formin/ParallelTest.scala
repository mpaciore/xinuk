package pl.edu.agh.formin

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.scalatest.concurrent.{Eventually, ScalaFutures}
import org.scalatest.{FlatSpec, Ignore, Matchers}
import pl.edu.agh.formin.algorithm.ForminMovesController
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.formin.model.{AlgaeAccessible, AlgaeCell, ForaminiferaAccessible, ForaminiferaCell}
import pl.edu.agh.xinuk.config.GuiType
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.{Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.simulation.WorkerActor

//todo review changes after clustering fix
@Ignore
class ParallelTest extends FlatSpec with Matchers with Eventually with ScalaFutures {
  implicit val config: ForminConfig = ForminConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppressionFactor = 0.5,
    signalAttenuationFactor = 1,
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 3,
    iterationsNumber = 3,
    isSupervisor = true,
    shardingMod = 1
  )

  trait Fixture {
    implicit val system: ActorSystem = ActorSystem("WorkerActorTest")
  }

  "A WorkerActors" should "have defined buffers correctly for worker surrounded by neighbours" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.BottomLeft),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopLeft),
        Neighbour(NeighbourPosition.TopRight),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.BottomRight)))
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || x == config.gridSize - 1 && y == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the left top corner of the grid" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.BottomRight))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the left bottom corner of the grid" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.TopRight))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the right bottom corner of the grid" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.TopLeft))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || y == 0
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the right top corner of the grid" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.BottomLeft))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == 0
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the top line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.BottomRight),
        Neighbour(NeighbourPosition.BottomLeft))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 && y != 0 && y != config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 && (y == 0 || y == config.gridSize - 1)
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the right line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopLeft),
        Neighbour(NeighbourPosition.BottomLeft))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && x != config.gridSize - 1 && y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == 0
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the left line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Bottom),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopRight),
        Neighbour(NeighbourPosition.BottomRight))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && x != config.gridSize - 1 && y == 0
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "have defined buffers correctly for worker in the bottom line of the grid not in the corner" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    worker ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Left),
        Neighbour(NeighbourPosition.Right),
        Neighbour(NeighbourPosition.Top),
        Neighbour(NeighbourPosition.TopRight),
        Neighbour(NeighbourPosition.TopLeft))
    )
    val grid: Grid = worker.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == config.gridSize - 1 && y != 0 && y != config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[BufferCell]
    }
  }

  it should "should make cells migrations correctly" in new Fixture {
    val workerRegion = TestProbe("workerRegion")
    val worker1 = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))
    val worker2 = TestActorRef(WorkerActor.props[ForminConfig](workerRegion.ref, (bufferZone, config) =>
      new ForminMovesController(bufferZone)(config), ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    ))

    worker1 ! WorkerActor.NeighboursInitialized(WorkerId(5),
      Vector(Neighbour(NeighbourPosition.Left))
    )

    worker2 ! WorkerActor.NeighboursInitialized(WorkerId(4),
      Vector(Neighbour(NeighbourPosition.Right))
    )

    val workers1Grid: Grid = worker1.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid
    workers1Grid.cells(1)(0) = BufferCell.apply(ForaminiferaAccessible.unapply(EmptyCell.Instance)
      .withForaminifera(config.foraminiferaStartEnergy, 0))
    workers1Grid.cells(2)(0) = BufferCell.apply(AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0))

    val workers2Grid: Grid = worker2.underlyingActor.asInstanceOf[WorkerActor[ForminConfig]].grid

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && x != config.gridSize - 1 && y != 0 && y != config.gridSize - 1
    } {
      workers2Grid.cells(x)(y) = EmptyCell.Instance
    }

    val bufferArray: Array[BufferCell] = NeighbourPosition.Left.bufferZone.iterator.map {
      case (x, y) => workers1Grid.cells(x)(y).asInstanceOf[BufferCell]
    }.toArray

    worker2 ! WorkerActor.IterationPartFinished(WorkerId(5), WorkerId(4), 1, bufferArray)

    workers2Grid.cells(1)(config.gridSize - 2) shouldBe an[ForaminiferaCell]
    workers2Grid.cells(2)(config.gridSize - 2) shouldBe an[AlgaeCell]
  }

}
