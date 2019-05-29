package pl.edu.agh.mock.utlis

import pl.edu.agh.mock.config.MockConfig
import pl.edu.agh.mock.model.{LocalPoint, MockCell}
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, Grid, Signal}

object SmellUtils{
  def calculateNeighboursSmell(cell: MockCell, x: Int, y: Int, grid: Grid, newGrid: Grid): Iterator[(Int, Int, Signal)] = {
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
        newGrid.cells(point._1)(point._2) match {
          case EmptyCell(_) => true
          case BufferCell(EmptyCell(_)) => true
          case BufferCell(MockCell(_,_,_,_)) => true
          case MockCell(_,_,_,_) => true
          case _ => false
        }
      })

//    neighbourCellCoordinates
//      .map {
//        case (i, j) => grid.cells(i)(j).smell.map{ case x => x.map{ y => y.value} }.sum()
//      }
//      .zipWithIndex
//      .iterator
//      .map {
//        case (smell, idx) =>
//          val (i, j) = neighbourCellCoordinates(idx)
//          (i, j, smell)
//      }
//      .filter(point =>{
//        newGrid.cells(point._1)(point._2) match {
//          case EmptyCell(_) => true
//          case BufferCell(EmptyCell(_)) => true
//          case BufferCell(MockCell(_,_,_,_)) => true
//          case MockCell(_,_,_,_) => true
//          case _ => false
//        }
//      })
  }


}