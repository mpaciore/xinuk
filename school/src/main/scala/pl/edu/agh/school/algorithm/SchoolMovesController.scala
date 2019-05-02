package pl.edu.agh.school.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.school.config.SchoolConfig
import pl.edu.agh.school.model._
import pl.edu.agh.school.simulation.SchoolMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics

import scala.collection.immutable.TreeSet
import scala.util.Random

final class SchoolMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: SchoolConfig) extends MovesController {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  def initialGrid3: (Grid, Metrics) = {
    grid = Grid.empty(bufferZone)

    var gridSize = config.gridSize

    // left - top
    grid.cells(gridSize / 2)(gridSize / 4) = TeacherAccessible.unapply(EmptyCell.Instance).withTeacher(Energy(0), 0)
    // left - bottom
    grid.cells(gridSize / 4)(gridSize / 4) = CleanerAccessible.unapply(EmptyCell.Instance).withCleaner(Energy(0), 0)

    // right - top
    grid.cells(gridSize / 4)(gridSize * 3 / 4) = StudentAccessible.unapply(EmptyCell.Instance).withStudent(0)
    // right - bottom
    grid.cells(gridSize / 2)(gridSize * 3 / 4) = DirtAccessible.unapply(EmptyCell.Instance).withDirt(Energy(0), 0)

    val metrics = SchoolMetrics(1, 1, 1, 0)
    (grid, metrics)
  }

  override def initialGrid: (Grid, SchoolMetrics) = {
    grid = Grid.empty(bufferZone)
    var cleanersCount = 0L
    var studentsCount = 0L
    var teachersCount = 0L
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        val chance = random.nextDouble()
        Math.abs(random.nextInt()) % 3 match {
          case 0 =>
            if (chance < config.cleanerSpawnChance) {
              cleanersCount += 1
              grid.cells(x)(y) = CleanerAccessible.unapply(EmptyCell.Instance).withCleaner(Energy(0), 0)
            }
          case 1 =>
            if (chance < config.teacherSpawnChance) {
              teachersCount += 1
              grid.cells(x)(y) = TeacherAccessible.unapply(EmptyCell.Instance).withTeacher(Energy(0), 0)
            }
          case 2 =>
            if (chance < config.studentSpawnChance) {
              studentsCount += 1
              grid.cells(x)(y) = StudentAccessible.unapply(EmptyCell.Instance).withStudent(0)
            }
        }
      }
    }
    val metrics = SchoolMetrics(studentsCount, teachersCount, cleanersCount, 0)
    // metrics are used to measure actual statistics for eg. current students count
    (grid, metrics)
  }


  def calculatePossibleDestinations(cell: CleanerCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    val tuples = Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .map {
        case (signalVector, index) => (signalVector(cell.signalIndex), index)
      }
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
    tuples
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def calculatePossibleDestinations(cell: TeacherCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .map {
        case (signalVector, index) => (signalVector(cell.signalIndex), index)
      }
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def calculatePossibleDestinations(cell: StudentCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .map {
        case (signalVector, index) => (signalVector(config.dirtSignalIndex), index) // make student go away from dirt
      }
      .sorted(implicitly[Ordering[(Signal, Int)]])
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
      .collectFirstOpt {
        case (i, j, currentCell@TeacherAccessible(_), TeacherAccessible(_)) =>
          (i, j, currentCell)
        case (i, j, currentCell@CleanerAccessible(_), CleanerAccessible(_)) =>
          (i, j, currentCell)
        case (i, j, currentCell@StudentAccessible(_), StudentAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  def selectStudentDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
      .filter(_ => new Random().nextBoolean())
      .collectFirstOpt {
        case (i, j, currentCell@StudentAccessible(_), StudentAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, SchoolMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone)

    var studentsCount = 0L
    var algaeCount = 0L
    var foraminiferaDeaths = 0L
    var foraminiferaReproductionsCount = 0L
    var consumedAlgaeCount = 0L
    var foraminiferaTotalLifespan = 0L
    var algaeTotalLifespan = 0L
    var foraminiferaTotalEnergy = 0.0

    def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
      grid.cells(i)(j) match {
        case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
        case _ => false
      }
    }

    //    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
    ////      val emptyCells =
    ////        Grid.neighbourCellCoordinates(x, y).flatMap {
    ////          case (i, j) =>
    ////            grid.cells(i)(j).opt
    ////              .filter(_ => creator.isDefinedAt(newGrid.cells(i)(j))) //use the same availability criteria on new grid
    ////              .collect(creator)
    ////              .map((i, j, _))
    ////        }
    ////      if (emptyCells.nonEmpty) {
    ////        val (newAlgaeX, newAlgaeY, newCell) = emptyCells(random.nextInt(emptyCells.size))
    ////        newGrid.cells(newAlgaeX)(newAlgaeY) = newCell
    ////      }
    ////    }

    def makeMove(x: Int, y: Int): Unit = {
      // order in this pattern match is important (cleaner and teacher must be last matched)
      this.grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }

        case cell: DirtCell =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: StudentCell =>
          moveStudent(cell, x, y)
        case cell: CleanerCell =>
          //          if (iteration % config.algaeReproductionFrequency == 0) {
          //            reproduce(x, y) { case AlgaeAccessible(accessible) => accessible.withAlgae(0) }
          //          }
          moveCleaner(cell, x, y)
        //          if (isEmptyIn(newGrid)(x, y)) {
        //            newGrid.cells(x)(y) = cell.copy(lifespan = cell.lifespan + 1)
        //          }
        case cell: TeacherCell =>
          //          if (cell.energy < config.foraminiferaLifeActivityCost) {
          //            killForaminifera(cell, x, y)
          //          } else if (cell.energy > config.foraminiferaReproductionThreshold) {
          //            reproduceForaminifera(cell, x, y)
          //          } else {
          moveTeacher(cell, x, y)
        //          }
      }
    }

    //    def killForaminifera(cell: CleanerCell, x: Int, y: Int): Unit = {
    //      foraminiferaDeaths += 1
    //      foraminiferaTotalLifespan += cell.lifespan
    //      val vacated = EmptyCell(cell.smell)
    //      newGrid.cells(x)(y) = vacated
    //      grid.cells(x)(y) = vacated
    //    }

    //    def reproduceForaminifera(cell: CleanerCell, x: Int, y: Int): Unit = {
    //      reproduce(x, y) { case DirtAccessible(accessible) => accessible.withDirt(config.foraminiferaStartEnergy, 0) }
    //      newGrid.cells(x)(y) = cell.copy(energy = cell.energy - config.foraminiferaReproductionCost, lifespan = cell.lifespan + 1)
    //      foraminiferaReproductionsCount += 1
    //    }

    def moveCleaner(cell: CleanerCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      destination match {
        case Opt((i, j, CleanerAccessible(dest))) =>
          val newCleaner = dest.withCleaner(cell.energy, cell.lifespan)
          newGrid.cells(i)(j) = newCleaner
          newGrid.cells(i)(j) match {
            case DirtCell(_, _, _, _) =>
              //                dirtCleaned += 1
              println("DIRT CLEANED!")
            case _ =>
          }
        case Opt((i, j, inaccessibleDestination)) =>
        //throw new RuntimeException(s"Cleaner selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.energy, cell.smell, cell.lifespan, cell.signalIndex)
      }
    }

    def moveTeacher(cell: TeacherCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      destination match {
        case Opt((i, j, TeacherAccessible(dest))) =>
          newGrid.cells(i)(j) = dest.withTeacher(cell.energy, cell.lifespan)
          newGrid.cells(i)(j) match {
            case StudentCell(_, _, _) =>
              // studentCleaned += 1
              println("STUDENT CAUGHT!")
            case _ =>
          }
        case Opt((i, j, inaccessibleDestination)) =>
        //throw new RuntimeException(s"Teacher selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.energy, cell.smell, cell.lifespan, cell.signalIndex)
      }
    }

    def moveStudent(cell: StudentCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectStudentDestinationCell(destinations, newGrid)
      destination match {
        case Opt((i, j, StudentAccessible(dest))) =>
          val studentLastCell = newGrid.cells(x)(y)
          if (new Random().nextInt(5) == 0) spawnDirtIfPossible(studentLastCell, x, y)
          newGrid.cells(i)(j) = dest.withStudent(cell.lifespan)
          newGrid.cells(i)(j) match {
            case DirtCell(_, _, _, _) =>
            case _ =>
          }
        case Opt((i, j, inaccessibleDestination)) =>
          throw new RuntimeException(s"Student selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.smell, cell.lifespan, cell.signalIndex)
      }
    }

    def spawnDirtIfPossible(cell: GridPart, x: Int, y: Int): Unit =
      cell match {
        case DirtAccessible(c) =>
          newGrid.cells(x)(y) = c.withDirt(Energy(0), 0)
        case _ =>
      }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      this.grid.cells(x)(y) match {
        case DirtCell(energy, _, _, _) =>
          foraminiferaTotalEnergy += energy.value
          studentsCount += 1
        case BufferCell(DirtCell(energy, _, _, _)) =>
          foraminiferaTotalEnergy += energy.value
          studentsCount += 1
        case StudentCell(_, _, _) | BufferCell(StudentCell(_, _, _)) =>
          algaeCount += 1
        case _ =>
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = SchoolMetrics(studentsCount, algaeCount, 0, foraminiferaDeaths)
    (newGrid, metrics)
  }
}
