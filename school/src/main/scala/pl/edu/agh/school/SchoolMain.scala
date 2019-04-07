package pl.edu.agh.school

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.school.algorithm.SchoolMovesController
import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.school.model.parallel.SchoolConflictResolver
import pl.edu.agh.school.model._
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, SmellingCell}

object SchoolMain extends LazyLogging {
  private val configPrefix = "school"
  private val metricHeaders = Vector( // TODO metric headers (it's just for information, but nonetheless...)
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
      case CleanerCell(_, _, _, _) => new Color(66, 134, 244)
      case DirtCell(_, _, _, _) => new Color(132, 86, 7)
      case StudentCell(_, _, _) => new Color(16, 234, 23)
      case TeacherCell(_, _, _, _) => new Color(132, 7, 7)
      case WallCell(_) => new Color(135, 135, 135)
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[SchoolConfig](configPrefix, metricHeaders, SchoolConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new SchoolMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }

}

