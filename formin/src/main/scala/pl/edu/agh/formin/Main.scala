package pl.edu.agh.formin

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.formin.algorithm.ForminMovesController
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.config.GuiType
import pl.edu.agh.xinuk.model.{Energy, Signal}

object Main extends LazyLogging {
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

  implicit val guiTypeReader: ValueReader[GuiType] =
    (config: Config, path: String) => GuiType.byName(config.getString(path))
  implicit val signalReader: ValueReader[Signal] =
    (config: Config, path: String) => Signal(config.getNumber(path).doubleValue())
  implicit val energyReader: ValueReader[Energy] =
    (config: Config, path: String) => Energy(config.getNumber(path).doubleValue())

  def main(args: Array[String]): Unit = {
    import net.ceedubs.ficus.Ficus._
    import net.ceedubs.ficus.readers.ArbitraryTypeReader._
    new Simulation(configPrefix, metricHeaders, ForminConflictResolver)(new ForminMovesController(_)(_)).start()
  }

}

