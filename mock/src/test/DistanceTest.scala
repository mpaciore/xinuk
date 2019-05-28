import org.scalatest.FunSuite
import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, Point}
import pl.edu.agh.mock.utlis.DistanceUtils
import pl.edu.agh.xinuk.model.WorkerId

class DistanceTest extends FunSuite{
  test("DistanceUtils.calculateGlobalPoint") {
    val point = LocalPoint(4, 2, WorkerId(3))
    val workersRoot = 2
    val gridSize = 10
    val globalPoint = Point(3, 12)
  }
}
