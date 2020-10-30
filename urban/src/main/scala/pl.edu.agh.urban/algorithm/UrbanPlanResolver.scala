package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.urban.model.UrbanCell
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, Update}
import pl.edu.agh.xinuk.model.{CellContents, Empty}

final case class UrbanPlanResolver() extends PlanResolver[UrbanConfig] {

  override def isUpdateValid(contents: CellContents, update: Update)(implicit config: UrbanConfig): Boolean = {
    contents match {
      case Empty => true
      case _ => false
    }
  }

  override def applyUpdate(contents: CellContents, update: Update)(implicit config: UrbanConfig): (CellContents, Metrics) = {
    val newContents = contents
    val metrics = update match {
      case _ => UrbanMetrics.empty
    }
    (newContents, metrics)
  }
}
