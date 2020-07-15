package pl.edu.agh.xinuk.model

trait SignalPropagation {
  def calculateUpdate(directions: Seq[Direction], neighbourSignals: Map[Direction, SignalMap]): SignalMap
}
