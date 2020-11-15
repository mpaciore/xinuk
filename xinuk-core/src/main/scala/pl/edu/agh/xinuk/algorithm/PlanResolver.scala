package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.CellContents

trait PlanResolver[Config <: XinukConfig] {
  def isUpdateValid(iteration: Long, contents: CellContents, update: Update)(implicit config: Config): Boolean

  def applyUpdate(iteration: Long, contents: CellContents, update: Update)(implicit config: Config): (CellContents, Metrics)
}
