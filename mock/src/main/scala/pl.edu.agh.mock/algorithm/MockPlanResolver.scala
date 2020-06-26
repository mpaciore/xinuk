package pl.edu.agh.mock.algorithm

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.MockCell
import pl.edu.agh.mock.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.PlanResolver
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Cell, Obstacle}

object MockPlanResolver extends PlanResolver[MockConfig] {
  override def isUpdateValid(update: Cell, target: Cell)(implicit config: MockConfig): Boolean = {
    (update, target) match {
      case (_, EmptyCell(_)) => true
      case _ => false
    }
  }

  override def applyUpdate(update: Cell, target: Cell)(implicit config: MockConfig): (Cell, MockMetrics) = {
    import pl.edu.agh.xinuk.model.Cell._
    val cell = update.withSmell(update.smell + target.smell)
    val metrics = update match {
      case MockCell(_) => MockMetrics(0, 1)
      case _ => MockMetrics.empty
    }
    (cell, metrics)
  }
}
