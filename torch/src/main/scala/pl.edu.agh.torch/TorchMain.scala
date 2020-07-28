package pl.edu.agh.torch

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.torch.algorithm.{TorchMetrics, TorchPlanCreator, TorchPlanResolver, TorchWorldCreator}
import pl.edu.agh.torch.model.{Exit, Fire, Person}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.grid.GridSignalPropagation
import pl.edu.agh.xinuk.model.{CellState, Obstacle, Signal}

object TorchMain extends LazyLogging {
  private val configPrefix = "torch"
  private val metricHeaders = Vector(
    "peopleCount",
    "fireCount",
    "exitCount",
    "peopleDeaths",
    "peopleEscapes"
  )

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      TorchWorldCreator,
      TorchPlanCreator,
      TorchPlanResolver,
      TorchMetrics.empty,
      GridSignalPropagation.Standard,
      {
        case cellState =>
          cellState.contents match {
            case Person(_) => Color.BLUE
            case Fire => Color.ORANGE
            case Exit => new Color(139, 69, 19)
            case Obstacle => Color.BLACK
            case _ => Color.WHITE
//            case _ => cellToColorSign(cellState)
          }
      }).start()
  }

  private def cellToColorSign(cellState: CellState): Color = {
    val maxSignal = cellState.signalMap
      .values
      .toSeq
      .sortBy(s => -Math.abs(s.value))
      .collectFirst({ case s: Signal => s.value })
      .getOrElse(0d)

    val brightness = Math.min(Math.max(0.2, Math.pow(Math.abs(maxSignal), 0.5)), 1).toFloat
    val saturation = 1f
    val hue = if (maxSignal >= 0) {
      1f
    } else {
      0.33f
    }
    Color.getHSBColor(hue, saturation, brightness)
  }
}

