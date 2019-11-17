package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.model.{Grid, WorkerId}
import pl.edu.agh.xinuk.simulation.Metrics

trait MovesController {
  def initialGrid(workerID: WorkerId): (Grid, Metrics, Any)

  def makeMoves(iteration: Long, grid: Grid): (Grid, Metrics)

  def receiveMessage(message: Any): Unit
}
