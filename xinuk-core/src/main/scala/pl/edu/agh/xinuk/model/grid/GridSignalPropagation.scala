package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellState, Direction, Signal, SignalMap, SignalPropagation}

object GridSignalPropagation {

  final val Standard: SignalPropagation = GridSignalPropagationStandard
  final val Bending: SignalPropagation = GridSignalPropagationBending

  @inline private def getPropagatedSignal(neighbourStates: Map[Direction, CellState], neighbourDirection: Direction, signalDirection: Direction)
                                         (implicit config: XinukConfig): Signal = {
    neighbourStates.get(neighbourDirection).map(_.signalMap(signalDirection)).getOrElse(Signal.zero)
  }

  @inline private def getGeneratedSignal(neighbourStates: Map[Direction, CellState], neighbourDirection: Direction, iteration: Long)
                                        (implicit config: XinukConfig): Signal = {
    neighbourStates.get(neighbourDirection).map(_.contents.generateSignal(iteration)).getOrElse(Signal.zero)
  }

  private final object GridSignalPropagationStandard extends SignalPropagation {
    def calculateUpdate(iteration: Long, neighbourStates: Map[Direction, CellState])(implicit config: XinukConfig): SignalMap = {
      config.worldType.directions.map({
        case cardinal@(GridDirection.Top | GridDirection.Right | GridDirection.Bottom | GridDirection.Left) =>
          (
            cardinal,
            cardinal.withAdjacent.map { d => getPropagatedSignal(neighbourStates, cardinal, d) }.reduce(_ + _) +
              getGeneratedSignal(neighbourStates, cardinal, iteration)
          )
        case diagonal@(GridDirection.TopLeft | GridDirection.TopRight | GridDirection.BottomRight | GridDirection.BottomLeft) =>
          (
            diagonal,
            getPropagatedSignal(neighbourStates, diagonal, diagonal) +
              getGeneratedSignal(neighbourStates, diagonal, iteration)
          )
        case direction => (direction, Signal.zero)
      }).toMap
    }
  }

  private final object GridSignalPropagationBending extends SignalPropagation {
    def direct: Double = 0.42
    def adjacent: Double = 0.29

    def calculateUpdate(iteration: Long, neighbourStates: Map[Direction, CellState])(implicit config: XinukConfig): SignalMap = {
      config.worldType.directions.map(direction =>
        (direction,
          getPropagatedSignal(neighbourStates, direction, direction) * direct +
            direction.adjacent.map { d => getPropagatedSignal(neighbourStates, direction, d) }.reduce(_ + _) * adjacent +
            getGeneratedSignal(neighbourStates, direction, iteration)
        )
      ).toMap
    }
  }

}
