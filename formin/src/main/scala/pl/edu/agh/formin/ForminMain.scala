package pl.edu.agh.formin

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.algorithm.ForminMovesController
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.xinuk.Simulation

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

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[ForminConfig](configPrefix, metricHeaders, ForminConflictResolver)(new ForminMovesController(_)(_)).start()
  }

}

