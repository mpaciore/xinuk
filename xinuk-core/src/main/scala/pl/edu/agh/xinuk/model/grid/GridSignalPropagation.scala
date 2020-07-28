package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{Direction, Signal, SignalMap, SignalPropagation}

object GridSignalPropagation {

  final val Standard: SignalPropagation = GridSignalPropagationStandard
    final val Circular: SignalPropagation = GridSignalPropagationCircular

  private final object GridSignalPropagationStandard extends SignalPropagation {
    def calculateUpdate(neighbourSignals: Map[Direction, SignalMap])(implicit config: XinukConfig): SignalMap = {
      config.worldType.directions.map({
        case cardinal@(GridDirection.Top | GridDirection.Right | GridDirection.Bottom | GridDirection.Left) =>
          (
            cardinal,
            cardinal.withAdjacent.map { d => neighbourSignals.get(cardinal).map(_ (d)).getOrElse(Signal.zero) }.reduce(_ + _)
          )
        case diagonal@(GridDirection.TopLeft | GridDirection.TopRight | GridDirection.BottomRight | GridDirection.BottomLeft) =>
          (
            diagonal,
            neighbourSignals.get(diagonal).map(_ (diagonal)).getOrElse(Signal.zero)
          )
        case direction => (direction, Signal.zero)
      }).toMap
    }
  }

  private final object GridSignalPropagationCircular extends SignalPropagation {
    def sideToSide: Double = 1.0 / 3

    def sideToCorner: Double = 1.0 / Math.sqrt(10)

    def cornerToSide: Double = 1.0 / Math.sqrt(13)

    def cornerToCorner: Double = 1.0 / (3 * Math.sqrt(2))

    def calculateUpdate(neighbourSignals: Map[Direction, SignalMap])(implicit config: XinukConfig): SignalMap = {
      config.worldType.directions.map({
        case cardinal@(GridDirection.Top | GridDirection.Right | GridDirection.Bottom | GridDirection.Left) =>
          (
            cardinal,
            neighbourSignals.get(cardinal).map(_ (cardinal) * sideToSide).getOrElse(Signal.zero) +
              cardinal.adjacent.map { d => neighbourSignals.get(cardinal).map(_ (d) * cornerToSide).getOrElse(Signal.zero) }.reduce(_ + _)
          )
        case diagonal@(GridDirection.TopLeft | GridDirection.TopRight | GridDirection.BottomRight | GridDirection.BottomLeft) =>
          (
            diagonal,
            neighbourSignals.get(diagonal).map(_ (diagonal) * cornerToCorner).getOrElse(Signal.zero) +
              diagonal.adjacent.map { d => neighbourSignals.get(diagonal).map(_ (d) * sideToCorner).getOrElse(Signal.zero) }.reduce(_ + _)
          )
        case direction => (direction, Signal.zero)
      }).toMap
    }
  }

}
