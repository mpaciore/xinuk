package pl.edu.agh.mock.utils

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, Signal}

// TODO: refactor name "SMELL"

object SmellUtils{
  def calculateNeighboursSmell(cell: MockCell, x: Int, y: Int, grid: Grid, newGrid: Grid): Iterator[(Int, Int, Signal)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)

    val smellsValuesList = neighbourCellCoordinates
      .map {
        case (i, j) => (i, j, Signal(grid.cells(x)(y).smell(i - x + 1)(j - y + 1).value))
      }

    val smellsList =
      smellsValuesList
        .map {
          case (_, _, smell) => smell.value
        }

    val (min, max) = smellsList.foldLeft((smellsList(0), smellsList(0))) {
      case ((min, max), e) => (if (min < e) min else e, if (max > e) max else e)
    }

//    wyłaczenie sprowadzania do od 0 do 1
//    val (min, max) = (0,1)

    smellsValuesList
      .map {
        case (i, j, smell) =>
          (i, j, Signal( (smell.value - min) / (max - min) ))
      }
      .iterator
      .filter(point => {
        newGrid.cells(point._1)(point._2) match {
          case EmptyCell(_) => true
          case BufferCell(EmptyCell(_)) => true
          case BufferCell(MockCell(_,_,_,_)) => true
          case MockCell(_,_,_,_) => true
          case _ => false
        }
    })
  }


}
