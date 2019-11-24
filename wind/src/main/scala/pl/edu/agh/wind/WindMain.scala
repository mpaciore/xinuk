package pl.edu.agh.wind

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.wind.algorithm.WindMovesController
import pl.edu.agh.wind.model.WindSourceCell
import pl.edu.agh.wind.model.parallel.WindConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

object WindMain extends LazyLogging {
  private val configPrefix = "wind"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      WindConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsCurved
    )(
      new WindMovesController(_)(_),
      {
        case WindSourceCell(_) => Color.WHITE
        case Obstacle => Color.GRAY
        case cell: SmellingCell => colorSmell(cell: SmellingCell)
        case _ => Color.BLACK
      }
    ).start()
  }

  def colorSmell(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat

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

}

