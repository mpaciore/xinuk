package pl.edu.agh.urban

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.urban.algorithm.{UrbanMetrics, UrbanPlanCreator, UrbanPlanResolver, UrbanWorldCreator}
import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.grid.GridSignalPropagation

object UrbanMain extends LazyLogging {
  private val configPrefix = "urban"

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      UrbanMetrics.MetricHeaders,
      UrbanWorldCreator,
      UrbanPlanCreator,
      UrbanPlanResolver,
      UrbanMetrics.empty,
      GridSignalPropagation.Bending,
      UrbanConfig.cellToColor
      ).start()
  }
}

