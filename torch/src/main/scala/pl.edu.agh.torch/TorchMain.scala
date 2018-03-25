package pl.edu.agh.torch

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.torch.algorithm.TorchMovesController
import pl.edu.agh.torch.model.parallel.TorchConflictResolver
import pl.edu.agh.xinuk.Simulation

object TorchMain extends LazyLogging {
  private val configPrefix = "torch"
  private val metricHeaders = Vector(
    "peopleCount",
    "fireCount",
    "escapeCount",
    "peopleDeaths"
  )

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(configPrefix, metricHeaders, TorchConflictResolver)(new TorchMovesController(_)(_)).start()
  }

}

