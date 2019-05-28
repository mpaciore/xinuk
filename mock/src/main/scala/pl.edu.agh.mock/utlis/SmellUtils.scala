package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, Signal}

object SmellUtils{
  def calculateNeighboursSmell(cell: MockCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, Signal)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map {
        case (i, j) => cell.smell(i)(j)
      }
      .zipWithIndex
      .iterator
      .map {
        case (smell, idx) =>
          val (i, j) = neighbourCellCoordinates(idx)
          (i, j, smell)
      }
      .filter(point =>{
        grid.cells(point._1,point._2) match {
          case EmptyCell(_) => true
          case BufferCell(EmptyCell(_)) => true
          case _ => false
        }
      })
  }


}
