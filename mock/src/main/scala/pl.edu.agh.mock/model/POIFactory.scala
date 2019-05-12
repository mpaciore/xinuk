package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.{EmptyCell, Grid}

import scala.util.Random


object POIFactory {
  def generatePOI(grid: Grid): (Int, Int) = {

    val random = new Random(System.nanoTime())
    var xDestination: Int = 0
    var yDestination: Int = 0

    do {
      xDestination = random.nextInt(config.gridSize)
      yDestination = random.nextInt(config.gridSize)
    } while (grid.cells(xDestination)(yDestination) match {
      case EmptyCell(_) =>
        false
      case _ => true
    }
    )
    (xDestination, yDestination)
  }
}
