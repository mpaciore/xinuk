package pl.edu.agh.formin.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.simulation.ForminMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class ForminMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: ForminConfig) extends MovesController {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, ForminMetrics) = {
    grid = Grid.empty(bufferZone)
    var foraminiferaCount = 0L
    var algaeCount = 0L
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grid.cells(x)(y) =
          if (random.nextDouble() < config.foraminiferaSpawnChance) {
            foraminiferaCount += 1
            ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
          }
          else {
            algaeCount += 1
            AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
          }
      }
    }
    val metrics = ForminMetrics(foraminiferaCount, algaeCount, 0, config.foraminiferaStartEnergy.value * foraminiferaCount, 0, 0, 0, 0)
    (grid, metrics)
  }


  def calculatePossibleDestinations(cell: ForaminiferaCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
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
        case (i, j, currentCell@ForaminiferaAccessible(_), ForaminiferaAccessible(_)) =>
          (i, j, currentCell)
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, ForminMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone)

    var foraminiferaCount = 0L
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

    def reproduce(x: Int, y: Int)(creator: PartialFunction[GridPart, GridPart]): Unit = {
      val emptyCells =
        Grid.neighbourCellCoordinates(x, y).flatMap {
          case (i, j) =>
            grid.cells(i)(j).opt
              .filter(_ => creator.isDefinedAt(newGrid.cells(i)(j))) //use the same availability criteria on new grid
              .collect(creator)
              .map((i, j, _))
        }
      if (emptyCells.nonEmpty) {
        val (newAlgaeX, newAlgaeY, newCell) = emptyCells(random.nextInt(emptyCells.size))
        newGrid.cells(newAlgaeX)(newAlgaeY) = newCell
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      this.grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case cell: AlgaeCell =>
          if (iteration % config.algaeReproductionFrequency == 0) {
            reproduce(x, y) { case AlgaeAccessible(accessible) => accessible.withAlgae(0) }
          }
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell.copy(lifespan = cell.lifespan + 1)
          }
        case cell: ForaminiferaCell =>
          if (cell.energy < config.foraminiferaLifeActivityCost) {
            killForaminifera(cell, x, y)
          } else if (cell.energy > config.foraminiferaReproductionThreshold) {
            reproduceForaminifera(cell, x, y)
          } else {
            moveForaminifera(cell, x, y)
          }
      }
    }

    def killForaminifera(cell: ForaminiferaCell, x: Int, y: Int): Unit = {
      foraminiferaDeaths += 1
      foraminiferaTotalLifespan += cell.lifespan
      val vacated = EmptyCell(cell.smell)
      newGrid.cells(x)(y) = vacated
      grid.cells(x)(y) = vacated
    }

    def reproduceForaminifera(cell: ForaminiferaCell, x: Int, y: Int): Unit = {
      reproduce(x, y) { case ForaminiferaAccessible(accessible) => accessible.withForaminifera(config.foraminiferaStartEnergy, 0) }
      newGrid.cells(x)(y) = cell.copy(energy = cell.energy - config.foraminiferaReproductionCost, lifespan = cell.lifespan + 1)
      foraminiferaReproductionsCount += 1
    }

    def moveForaminifera(cell: ForaminiferaCell, x: Int, y: Int): Unit = {
      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      destination match {
        case Opt((_, _, AlgaeCell(_, lifespan))) =>
          consumedAlgaeCount += 1
          algaeTotalLifespan += lifespan
        case Opt((_, _, BufferCell(AlgaeCell(_, lifespan)))) =>
          consumedAlgaeCount += 1
          algaeTotalLifespan += lifespan
        case _ =>
      }
      destination match {
        case Opt((i, j, ForaminiferaAccessible(destination))) =>
          newGrid.cells(i)(j) = destination.withForaminifera(cell.energy - config.foraminiferaLifeActivityCost, cell.lifespan + 1)
          val vacated = EmptyCell(cell.smell)
          newGrid.cells(x)(y) = vacated
          grid.cells(x)(y) = vacated
        case Opt((i, j, inaccessibleDestination)) =>
          throw new RuntimeException(s"Foraminifera selected inaccessible destination ($i,$j): $inaccessibleDestination")
        case Opt.Empty =>
          newGrid.cells(x)(y) = cell.copy(cell.energy - config.foraminiferaLifeActivityCost, lifespan = cell.lifespan + 1)
          grid.cells(x)(y)
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      this.grid.cells(x)(y) match {
        case ForaminiferaCell(energy, _, _) =>
          foraminiferaTotalEnergy += energy.value
          foraminiferaCount += 1
        case BufferCell(ForaminiferaCell(energy, _, _)) =>
          foraminiferaTotalEnergy += energy.value
          foraminiferaCount += 1
        case AlgaeCell(_, _) | BufferCell(AlgaeCell(_, _)) =>
          algaeCount += 1
        case _ =>
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = ForminMetrics(foraminiferaCount, algaeCount, foraminiferaDeaths, foraminiferaTotalEnergy, foraminiferaReproductionsCount, consumedAlgaeCount, foraminiferaTotalLifespan, algaeTotalLifespan)
    (newGrid, metrics)
  }
}