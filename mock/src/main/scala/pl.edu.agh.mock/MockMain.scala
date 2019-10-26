package pl.edu.agh.mock

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.algorithm.MockMovesController
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.mock.model.parallel.MockConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

object MockMain extends LazyLogging {
  private val configPrefix = "mock"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      MockConflictResolver,
      DefaultSmellPropagation.calculateSmellAddends)(new MockMovesController(_)(_),
      {
        case MockCell(_, x, _,_) =>
          x.size match {
            case 0 => Color.white
            case 1 => Color.yellow
            case 2 => Color.pink
            case _ => Color.cyan
          }
        case Obstacle => Color.black
        case cell: SmellingCell => cellToColorRegions(cell)
      }).start()
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).sum).sum.toFloat
    val brightness = 1.toFloat - Math.pow(smellValue, 0.1).toFloat
    val saturation = 0f
  /*  if (smellValue < 0.00001) {
      val hue = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }*/
    val hue = 0f
    Color.getHSBColor(hue, saturation, brightness)
  }

}

