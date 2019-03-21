package pl.edu.agh.formin

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.algorithm.ForminMovesController
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.formin.model.{AlgaeCell, ForaminiferaCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.SmellingCell

object ForminMain extends LazyLogging {
  private val configPrefix = "formin"
  private val metricHeaders = Vector(
    "foraminiferaCount",
    "algaeCount",
    "foraminiferaDeaths",
    "foraminiferaTotalEnergy",
    "foraminiferaReproductionsCount",
    "consumedAlgaeCount",
    "foraminiferaTotalLifespan",
    "algaeTotalLifespan"
  )

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case AlgaeCell(_, _) => new Color(0, 128, 0)
      case ForaminiferaCell(_, _, _) => new Color(139, 69, 19)
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[ForminConfig](configPrefix, metricHeaders, ForminConflictResolver)(new ForminMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }

}

