package pl.edu.agh.urban.algorithm

import pl.edu.agh.urban.config.UrbanConfig
import pl.edu.agh.urban.model.UrbanCell
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, CellState, Empty}

final case class UrbanPlanResolver() extends PlanResolver[UrbanConfig] {

  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: UrbanConfig): Boolean = {
    contents match {
      case Empty => true
      case _ => false
    }
  }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: UrbanConfig): (CellContents, Metrics) = {
    val newContents = update.value
    val metrics = update.value match {
      case _: UrbanCell => UrbanMetrics(0, 1)
      case _ => UrbanMetrics.empty
    }
    (newContents, metrics)
  }
}
