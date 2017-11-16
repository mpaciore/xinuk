package pl.edu.agh.formin

import org.scalatest.{BeforeAndAfter, FlatSpecLike, Matchers}
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model.parallel.NeighbourPosition
import pl.edu.agh.formin.model.{Energy, Signal}

class NeighbourTest extends FlatSpecLike with Matchers with BeforeAndAfter {
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
    workersRoot = 1,
    iterationsNumber = 1000,
    isSupervisor = true
  )

  "An affectedCells method" should "return correct affected cells coordinates for TOP one" in {
    val affectedCells = NeighbourPosition.Top.affectedCells.toVector
    affectedCells(0)._1 shouldBe 1
    affectedCells(0)._2 shouldBe 0
    affectedCells(1)._1 shouldBe 1
    affectedCells(1)._2 shouldBe 1
    affectedCells(2)._1 shouldBe 1
    affectedCells(2)._2 shouldBe 2
    affectedCells(3)._1 shouldBe 1
    affectedCells(3)._2 shouldBe 3
    affectedCells(4)._1 shouldBe 1
    affectedCells(4)._2 shouldBe 4
  }

  it should "return correct affected cells coordinates for LEFT one" in {
    val affectedCells = NeighbourPosition.Left.affectedCells.toVector
    affectedCells(0)._1 shouldBe 0
    affectedCells(0)._2 shouldBe 1
    affectedCells(1)._1 shouldBe 1
    affectedCells(1)._2 shouldBe 1
    affectedCells(2)._1 shouldBe 2
    affectedCells(2)._2 shouldBe 1
    affectedCells(3)._1 shouldBe 3
    affectedCells(3)._2 shouldBe 1
    affectedCells(4)._1 shouldBe 4
    affectedCells(4)._2 shouldBe 1
  }
}
