package pl.edu.agh.xinuk.model

import pl.edu.agh.xinuk.config.XinukConfig

trait SignalPropagation {
  def calculateUpdate(neighbourSignals: Map[Direction, SignalMap])(implicit config: XinukConfig): SignalMap
}
