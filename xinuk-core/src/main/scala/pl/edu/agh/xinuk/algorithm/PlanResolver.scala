package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.CellContents

trait PlanResolver[Config <: XinukConfig] {
  def isUpdateValid(contents: CellContents, update: Update)(implicit config: Config): Boolean

  def applyUpdate(contents: CellContents, update: Update)(implicit config: Config): (CellContents, Metrics)
}
