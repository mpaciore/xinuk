package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig

trait SignalPropagation {
  def calculateUpdate(iteration: Long, neighbourStates: Map[Direction, CellState])(implicit config: XinukConfig): SignalMap
}
