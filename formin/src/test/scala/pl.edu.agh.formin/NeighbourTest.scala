package pl.edu.agh.formin

import com.avsystem.commons.misc.Opt
import org.scalatest.{BeforeAndAfter, FlatSpecLike, Matchers}
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.xinuk.config.GuiType
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.NeighbourPosition

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
    workersRoot = 3,
    iterationsNumber = 1000,
    isSupervisor = true,
    shardingMod = 1
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

  "An neighbourId method" should "return correct neighbour id for TOP one" in {
    val topNeighourId = NeighbourPosition.Top.neighbourId(WorkerId(5))
    topNeighourId shouldBe Opt(WorkerId(2))
  }

  it should "return correct neighbour id for LEFT one" in {
    val topNeighourId = NeighbourPosition.Left.neighbourId(WorkerId(5))
    topNeighourId shouldBe Opt(WorkerId(4))
  }

  it should "return correct neighbour id for TOP LEFT one" in {
    val topNeighourId = NeighbourPosition.TopLeft.neighbourId(WorkerId(5))
    topNeighourId shouldBe Opt(WorkerId(1))
  }

  it should "return correct neighbour id for BOTTOM RIGHT one" in {
    val topNeighourId = NeighbourPosition.BottomRight.neighbourId(WorkerId(5))
    topNeighourId shouldBe Opt(WorkerId(9))
  }

  "A bufferZone method" should "return correct buffer zone for TOP one" in {
    val topBufferZone = NeighbourPosition.Top.bufferZone(config).toVector
    topBufferZone(0)._1 shouldBe 0
    topBufferZone(0)._2 shouldBe 0
    topBufferZone(4)._1 shouldBe 0
    topBufferZone(4)._2 shouldBe 4
  }

  it should "return correct buffer zone for LEFT one" in {
    val leftBufferZone = NeighbourPosition.Left.bufferZone(config).toVector
    leftBufferZone(0)._1 shouldBe 0
    leftBufferZone(0)._2 shouldBe 0
    leftBufferZone(4)._1 shouldBe 4
    leftBufferZone(4)._2 shouldBe 0
  }

  it should "return correct buffer zone for TOP LEFT one" in {
    val topLeftBufferZone = NeighbourPosition.TopLeft.bufferZone(config).toVector
    topLeftBufferZone(0)._1 shouldBe 0
    topLeftBufferZone(0)._2 shouldBe 0
  }

  it should "return correct buffer zone for BOTTOM RIGHT one" in {
    val bottomRightBufferZone = NeighbourPosition.BottomRight.bufferZone(config).toVector
    bottomRightBufferZone(0)._1 shouldBe 4
    bottomRightBufferZone(0)._2 shouldBe 4
  }
}