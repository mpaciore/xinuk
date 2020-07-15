package pl.edu.agh.fortwist

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.fortwist.algorithm.FortwistMovesController
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.FortwistCell
import pl.edu.agh.fortwist.model.parallel.FortwistConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.SignalPropagation

object FortwistMain extends LazyLogging {
  private val configPrefix = "fortwist"
  private val metricHeaders = Vector(
    "foraminiferaCount",
    "algaeCount",
    "foraminiferaDeaths",
    "foraminiferaTotalEnergy",
    "foraminiferaReproductionsCount",
    "consumedAlgaeCount",
    "foraminiferaTotalLifespan",
    "foraminiferaMoves"
  )

  private def cellToColor(cell: FortwistCell): Color = {
    val hue = 0.11f
    val saturation = 0.69f
    //val luminance = cell.algae.value.floatValue()
    val luminance = cell.foraminiferas.size / 10f
    Color.getHSBColor(hue, saturation, luminance)
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    import pl.edu.agh.fortwist.config.FortwistValueReaders._
    new Simulation[FortwistConfig](configPrefix, metricHeaders, FortwistConflictResolver,
      SignalPropagation.Standard, FortwistCell.create())(
      new FortwistMovesController(_)(_),
      { case cell: FortwistCell => cellToColor(cell) }
    ).start()
  }

}

