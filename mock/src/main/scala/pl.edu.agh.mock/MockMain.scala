package pl.edu.agh.mock

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.mock.algorithm.{MockPlanCreator, MockPlanResolver, MockWorldCreator}
import pl.edu.agh.mock.model.Mock
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.grid.GridSignalPropagation
import pl.edu.agh.xinuk.model.{CellState, Obstacle}

object MockMain extends LazyLogging {
  private val configPrefix = "mock"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    import pl.edu.agh.xinuk.model.grid.GridDirection._
    new Simulation(
      configPrefix,
      metricHeaders,
      MockWorldCreator,
      () => MockPlanCreator(),
      () => MockPlanResolver(),
      MockMetrics.empty,
      GridSignalPropagation.Circular,
      {
        case cell =>
          cell.contents match {
            case Mock => Color.WHITE
            case Obstacle => Color.BLUE
            case _ => cellToColorRegions(cell)
          }
      }).start()
  }

  private def cellToColorRegions(cellState: CellState): Color = {
    val smellValue = cellState.signalMap.values.map(_.value).max.toFloat
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

  private def cellToColor(cellState: CellState): Color = {
    val smellValue = cellState.signalMap.values.map(_.value).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    val hue = 1f
    val saturation = 0.69f
    Color.getHSBColor(hue, saturation, brightness)
  }

}

