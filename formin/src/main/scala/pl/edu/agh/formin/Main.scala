package pl.edu.agh.formin

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.algorithm.ForminMovesController
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.xinuk.Simulation

object Main extends LazyLogging {
  final val ForminConfigPrefix = "formin"
  final val MetricHeaders = Vector(
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
    new Simulation(ForminConfigPrefix, MetricHeaders, ForminConflictResolver)(
      new ForminMovesController(_, _)(_)
    )(ForminConfig.fromConfig).start()
  }

}

