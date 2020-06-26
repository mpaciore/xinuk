package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.Cell
import pl.edu.agh.xinuk.simulation.Metrics;

trait PlanResolver[T <: XinukConfig] {
  def isUpdateValid(update: Cell, target: Cell)(implicit config: T): Boolean

  def applyUpdate(update: Cell, target: Cell)(implicit config: T): (Cell, Metrics)
}
