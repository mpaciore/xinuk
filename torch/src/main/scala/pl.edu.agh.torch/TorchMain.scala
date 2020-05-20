package pl.edu.agh.torch

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.torch.algorithm.{TorchGridCreator, TorchMovesController}
import pl.edu.agh.torch.model.parallel.TorchConflictResolver
import pl.edu.agh.torch.model.{EscapeCell, FireCell, HumanCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, GridPart}

object TorchMain extends LazyLogging {
  private val configPrefix = "torch"
  private val metricHeaders = Vector(
    "peopleCount",
    "fireCount",
    "escapeCount",
    "peopleDeaths",
    "peopleEscapes"
  )

  private def cellToColor(cell: GridPart): Color = {
    cell match {
      case HumanCell(_, _, _) => Color.BLUE
      case FireCell(_) => Color.ORANGE
      case EscapeCell(_) => new Color(139, 69, 19)
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      TorchConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard
    )(
      TorchGridCreator.apply(_),
      TorchMovesController.apply(_),
      { case cell: GridPart => cellToColor(cell) }
    ).start()
  }

}

