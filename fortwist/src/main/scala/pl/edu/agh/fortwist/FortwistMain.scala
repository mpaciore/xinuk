package pl.edu.agh.fortwist

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.fortwist.algorithm.{FortwistMetrics, FortwistPlanCreator, FortwistPlanResolver, FortwistWorldCreator}
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.Seabed
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.CellState
import pl.edu.agh.xinuk.model.grid.GridSignalPropagation

object FortwistMain extends LazyLogging {
  private val configPrefix = "fortwist"

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[FortwistConfig](
      configPrefix,
      FortwistMetrics.MetricHeaders,
      FortwistWorldCreator,
      FortwistPlanCreator,
      FortwistPlanResolver,
      FortwistMetrics.empty,
      GridSignalPropagation.Standard,
      cellToColor
    ).start()
  }

  private def cellToColor: PartialFunction[CellState, Color] = {
    case CellState(Seabed(foraminiferas, algae), _) =>
      val hue = 0.11f
      val saturation = 0.69f
      //  val luminance = algae.toFloat
      val luminance = foraminiferas.size / 10f
      Color.getHSBColor(hue, saturation, luminance)
  }
}
