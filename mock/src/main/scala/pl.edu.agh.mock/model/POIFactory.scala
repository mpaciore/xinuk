package pl.edu.agh.mock.model

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid}

import scala.util.Random


object POIFactory {
  def generatePOI(grid: Grid): Point = {

    val random = new Random(System.nanoTime())
    var xDestination: Int = 0
    var yDestination: Int = 0

    do {
      xDestination = random.nextInt(grid.cells.length)
      yDestination = random.nextInt(grid.cells.length)
    } while (grid.cells(xDestination)(yDestination) match {
      case EmptyCell(_) => false
      case BufferCell(EmptyCell(_)) => false
      case _ => true
    }
    )
    Point(xDestination, yDestination)
  }
}
