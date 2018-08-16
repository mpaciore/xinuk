package pl.edu.agh.xinuk.model.parallel

import com.avsystem.commons.misc.Opt
import org.mockito.Mockito
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.WorkerId

class NeighbourPositionTest extends FlatSpec with Matchers with BeforeAndAfter with MockitoSugar {
  implicit val config: XinukConfig = mock[XinukConfig]
  Mockito.when(config.gridSize).thenAnswer(new Answer[Int] {
    override def answer(invocation: InvocationOnMock): Int = 5
  })
  Mockito.when(config.workersRoot).thenAnswer(new Answer[Int] {
    override def answer(invocation: InvocationOnMock): Int = 3
  })

  "An affectedCells method" should "return correct affected cells coordinates for TOP one" in {
    val affectedCells = NeighbourPosition.Top.affectedCells.toVector
    affectedCells should contain inOrderOnly((1, 1), (1, 2), (1, 3))
  }

  it should "return correct affected cells coordinates for LEFT one" in {
    val affectedCells = NeighbourPosition.Left.affectedCells.toVector
    affectedCells should contain inOrderOnly((1, 1), (2, 1), (3, 1))
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
    topBufferZone should contain inOrderOnly((0, 1), (0, 2), (0, 3))
  }

  it should "return correct buffer zone for LEFT one" in {
    val leftBufferZone = NeighbourPosition.Left.bufferZone(config).toVector
    leftBufferZone should contain inOrderOnly((1, 0), (2, 0), (3, 0))
  }

  it should "return correct buffer zone for TOP LEFT one" in {
    val topLeftBufferZone = NeighbourPosition.TopLeft.bufferZone(config).toVector
    topLeftBufferZone should contain only ((0, 0))
  }

  it should "return correct buffer zone for BOTTOM RIGHT one" in {
    val bottomRightBufferZone = NeighbourPosition.BottomRight.bufferZone(config).toVector
    bottomRightBufferZone should contain only ((4, 4))
  }
}