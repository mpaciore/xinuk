package pl.edu.agh.fortwist.algorithm

import com.avsystem.commons
import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.Opt
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.{Foraminifera, FortwistCell}
import pl.edu.agh.fortwist.simulation.FortwistMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class FortwistMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: FortwistConfig)
  extends MovesController {

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, FortwistMetrics) = {
    grid = Grid.empty(bufferZone)
    var foraminiferaCount = 0L
    var algaeCount = 0.0
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      val foraminiferas: Vector[Foraminifera] =
        if (random.nextDouble() < config.foraminiferaSpawnChance) Vector(Foraminifera.create())
        else Vector.empty
      val cell = FortwistCell.create(foraminiferas)
      foraminiferaCount += foraminiferas.size
      algaeCount += cell.algae.value
      grid.cells(x)(y) = cell
    }
    val metrics = FortwistMetrics(
      foraminiferaCount = foraminiferaCount,
      algaeCount = algaeCount,
      foraminiferaDeaths = 0,
      foraminiferaTotalEnergy = config.foraminiferaStartEnergy.value * foraminiferaCount,
      foraminiferaReproductionsCount = 0,
      consumedAlgaeCount = 0,
      foraminiferaTotalLifespan = 0
    )
    (grid, metrics)
  }

  def selectDestinationCell(
    possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
      .collectFirstOpt {
        case x => ???
      }
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, FortwistMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone)

    var foraminiferaCount = 0L
    var algaeCount = 0.0
    var foraminiferaDeaths = 0L
    var foraminiferaReproductionsCount = 0L
    var consumedAlgaeCount = 0.0
    var foraminiferaTotalLifespan = 0L
    var foraminiferaTotalEnergy = 0.0

    def makeMove(x: Int, y: Int): Unit = {
      def isEmptyIn(grid: Grid)(i: Int, j: Int): Boolean = {
        grid.cells(i)(j) match {
          case EmptyCell(_) | BufferCell(EmptyCell(_)) => true
          case _ => false
        }
      }

      this.grid.cells(x)(y) match {
        case Obstacle =>
          newGrid.cells(x)(y) = Obstacle
        case cell@(EmptyCell(_) | BufferCell(_)) =>
          if (isEmptyIn(newGrid)(x, y)) {
            newGrid.cells(x)(y) = cell
          }
        case FortwistCell(smell, foraminiferas, algae) =>
        /*case cell: AlgaeCell =>
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
          }*/
      }
    }

    def killForaminifera(foraminifera: Foraminifera): Iterator.empty.type = {
      foraminiferaDeaths += 1
      foraminiferaTotalLifespan += foraminifera.lifespan
      Iterator.empty
    }

    def reproduceForaminifera(formin: Foraminifera): Iterator[Foraminifera] = {
      val child = Foraminifera.create()
      val parent = formin.copy(energy = formin.energy - config.foraminiferaReproductionCost, lifespan = formin.lifespan + 1)
      foraminiferaReproductionsCount += 1
      Iterator(parent, child)
    }

    def eatAlgae(cell: Foraminifera): (Iterator[Foraminifera], Energy) = {
      val energyChange = config.algaeEnergeticCapacity
      consumedAlgaeCount += energyChange.value
      val afterEating = cell.copy(energy = cell.energy + energyChange, lifespan = cell.lifespan + 1)
      (Iterator(afterEating), energyChange)
    }

    def moveForaminifera(foraminifera: Foraminifera, x: Int, y: Int): (Iterator[Foraminifera], Iterator[(Foraminifera, Int, Int)]) = {
      val destinations = calculatePossibleDestinations(x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)
      val afterMoving = foraminifera.copy(
        energy = foraminifera.energy - config.foraminiferaLifeActivityCost,
        lifespan = foraminifera.lifespan + 1
      )
      destination match {
        case Opt((i, j, _)) =>
          val oldPlace = Iterator.empty
          val newPlace = Iterator((afterMoving, i, j))
          (oldPlace, newPlace)
        case Opt.Empty =>
          (Iterator(afterMoving), Iterator.empty)
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    //todo metrics before a move
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      newGrid.cells(x)(y) match {
        case FortwistCell(smell, foraminiferas, algae) =>
          foraminiferaTotalEnergy += foraminiferas.iterator.map(_.energy.value).sum
          foraminiferaCount += foraminiferas.size
          algaeCount += algae.value
        case BufferCell(FortwistCell(smell, foraminiferas, algae)) =>
          foraminiferaTotalEnergy += foraminiferas.iterator.map(_.energy.value).sum
          foraminiferaCount += foraminiferas.size
          algaeCount += algae.value
        case _ =>
      }
    }
    val metrics = FortwistMetrics(
      foraminiferaCount = foraminiferaCount,
      algaeCount = algaeCount,
      foraminiferaDeaths = foraminiferaDeaths,
      foraminiferaTotalEnergy = foraminiferaTotalEnergy,
      foraminiferaReproductionsCount = foraminiferaReproductionsCount,
      consumedAlgaeCount = consumedAlgaeCount,
      foraminiferaTotalLifespan = foraminiferaTotalLifespan
    )
    (newGrid, metrics)
  }
}