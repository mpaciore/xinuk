package pl.edu.agh.formin

case class Grid(cells: Array[Array[GridCell]]) extends AnyVal

object Grid {
  def apply(n: Int): Grid = Grid(Array.ofDim[GridCell](n, n))
}

sealed trait GridCell {
  def effectiveSignal: Double
}

sealed trait TakenCell extends GridCell

sealed trait SignalSource extends TakenCell {
  def signalSource: Double

  def sourceTimeToLive: Int
}

case class ForaminiferaCell(effectiveSignal: Double, sourceTimeToLive: Int) extends SignalSource {
  override val signalSource: Double = -1d
}

case class AlgaeCell(effectiveSignal: Double, sourceTimeToLive: Int) extends SignalSource {
  override val signalSource: Double = 1d
}

case class NeighbourCell(effectiveSignal: Double, neighbourOf: SignalSource) extends TakenCell

case class EmptyCell(effectiveSignal: Double) extends GridCell