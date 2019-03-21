package pl.edu.agh.mock

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.algorithm.MockMovesController
import pl.edu.agh.mock.model.MockNonEmptyCell
import pl.edu.agh.mock.model.parallel.MockConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Obstacle, SmellingCell}

object MockMain extends LazyLogging {
  private val configPrefix = "mock"
  private val metricHeaders = Vector()


  private def maxSmell(smell: SmellArray): Float = {
    smell.map(_.map(_.value).max).max.toFloat
  }

  private def sumSmell(smell: SmellArray): Float = {
    smell.map(_.map(_.value).sum).sum.toFloat
  }

  private def smellFrom(i: Int, j: Int)(smell: SmellArray): Float = {
    smell(i)(j).value.toFloat
  }

  private def cellToColorRegions(smellExtractor: SmellArray => Float)(cell: SmellingCell): Color = {
    val smellValue = smellExtractor(cell.smell)
    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0.00001) {
      val hue = 1f
      val saturation = 1f
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
    }
  }

  private def cellToColor(smellExtractor: SmellArray => Float)(cell: SmellingCell): Color = {
    val smellValue = smellExtractor(cell.smell)
    val brightness = Math.pow(smellValue, 0.1).toFloat
    val hue = 1f
    val saturation = 0.69f
    Color.getHSBColor(hue, saturation, brightness)
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(configPrefix, metricHeaders, MockConflictResolver)(new MockMovesController(_)(_), {
      case MockNonEmptyCell(_) => Color.WHITE
      case Obstacle => Color.BLUE
      case cell: SmellingCell => cellToColorRegions(maxSmell)(cell)
    }).start()
  }

}

