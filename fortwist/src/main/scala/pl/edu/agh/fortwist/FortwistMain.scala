package pl.edu.agh.fortwist

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.fortwist.algorithm.FortwistMovesController
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.FortwistCell
import pl.edu.agh.fortwist.model.parallel.FortwistConflictResolver
import pl.edu.agh.xinuk.Simulation

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
    "foraminiferaMoves",
  )

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[FortwistConfig](configPrefix, metricHeaders, FortwistConflictResolver, FortwistCell.create())(
      new FortwistMovesController(_)(_)
    ).start()
  }

}

