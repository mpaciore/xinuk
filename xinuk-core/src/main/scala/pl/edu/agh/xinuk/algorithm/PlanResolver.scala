package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.CellState
import pl.edu.agh.xinuk.simulation.Metrics

trait PlanResolver[Config <: XinukConfig] {
  def isUpdateValid(state: CellState, update: StateUpdate)(implicit config: Config): Boolean

  def applyUpdate(state: CellState, update: StateUpdate)(implicit config: Config): (CellState, Metrics)
}
