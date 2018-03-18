package pl.edu.agh.fortwist.algorithm

import com.avsystem.commons._
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

  import Cell._

  private var grid: Grid = _

  private val random = new Random(System.nanoTime())

  override def initialGrid: (Grid, FortwistMetrics) = {
    grid = Grid.empty(bufferZone, FortwistCell(Cell.emptySignal, Vector.empty, config.algaeStartEnergy))
    var foraminiferaCount = 0L
    var algaeCount = 0.0
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y != 0 && x != config.gridSize - 1 && y != config.gridSize - 1
    } {
      if (random.nextDouble() < config.foraminiferaSpawnChance) {
        val foraminiferas = Vector(Foraminifera.create())
        val cell = FortwistCell(
          smell = Cell.emptySignal + config.foraminiferaInitialSignal + (config.algaeSignalMultiplier * config.algaeStartEnergy.value),
          foraminiferas = foraminiferas,
          algae = config.algaeStartEnergy
        )
        foraminiferaCount += foraminiferas.size
        algaeCount += cell.algae.value
        grid.cells(x)(y) = cell
      }
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

  override def makeMoves(iteration: Long, grid: Grid): (Grid, FortwistMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone, FortwistCell.create())

    var foraminiferaCount = 0L
    var algaeCount = 0.0
    var foraminiferaDeaths = 0L
    var foraminiferaReproductionsCount = 0L
    var consumedAlgaeCount = 0.0
    var foraminiferaTotalLifespan = 0L
    var foraminiferaTotalEnergy = 0.0
    var movesCount = 0L

    def update(x: Int, y: Int)(op: FortwistCell => FortwistCell): Unit = {
      val updated = op(newGrid.cells(x)(y).asInstanceOf[FortwistCell]) //the grid is initialized with FC
      val smellAdjustment = (config.foraminiferaInitialSignal * updated.foraminiferas.size) +
        (config.algaeSignalMultiplier * algaeCount)
      newGrid.cells(x)(y) = updated.copy(smell = updated.smell + smellAdjustment)
    }

    def makeMove(x: Int, y: Int): Unit = {
      this.grid.cells(x)(y) match {
        case Obstacle | BufferCell(_) =>
        case FortwistCell(smell, foraminiferas, algaeEnergy) => {
          val (newForaminiferas: Iterator[Foraminifera], moves: BMap[(Int, Int), Stream[Foraminifera]], newAlgaeEnergy: Energy) =
            foraminiferas.foldLeft(
              (Iterator[Foraminifera](), MMap.empty[(Int, Int), Stream[Foraminifera]].withDefaultValue(Stream.empty), algaeEnergy)
            ) { case ((currentCellResult, moves, algaeEnergy), formin) =>
              val action = if (formin.energy > config.foraminiferaReproductionThreshold) {
                reproduceForaminifera(formin)
              } else if (algaeEnergy > config.algaeEnergeticCapacity) {
                eatAlgae(formin)
              } else if (formin.energy < config.foraminiferaLifeActivityCost) {
                killForaminifera(formin)
              } else {
                //todo use moves to avoid grouping
                moveForaminifera(formin, x, y)
              }
              action.moves.foreach { case ((x, y), movingFormin) => moves((x, y)) = moves((x, y)) :+ movingFormin }
              (currentCellResult ++ action.currentCellResult, moves, algaeEnergy + action.algaeEnergyDiff)
            }
          import Cell._
          update(x, y)(f => f.copy(
            smell = f.smell + smell,
            foraminiferas = f.foraminiferas ++ newForaminiferas,
            algae = f.algae + newAlgaeEnergy + config.algaeRegenerationRate)
          )
          moves.foreach { case ((i, j), formins) =>
            movesCount += formins.size
            update(i, j)(f => f.copy(foraminiferas = f.foraminiferas ++ formins))
          }
        }
      }
    }

    final case class ForminAction(
      currentCellResult: Iterator[Foraminifera],
      algaeEnergyDiff: Energy = Energy.Zero,
      moves: Iterator[((Int, Int), Foraminifera)] = Iterator.empty,
    )

    def killForaminifera(foraminifera: Foraminifera): ForminAction = {
      foraminiferaDeaths += 1
      foraminiferaTotalLifespan += foraminifera.lifespan
      ForminAction(Iterator.empty)
    }

    def reproduceForaminifera(formin: Foraminifera): ForminAction = {
      val child = Foraminifera.create()
      val parent = formin.copy(energy = formin.energy - config.foraminiferaReproductionCost, lifespan = formin.lifespan + 1)
      foraminiferaReproductionsCount += 1
      ForminAction(Iterator(parent, child))
    }

    def eatAlgae(formin: Foraminifera): ForminAction = {
      val energyChange = config.algaeEnergeticCapacity
      consumedAlgaeCount += energyChange.value
      val afterEating = formin.copy(energy = formin.energy + energyChange, lifespan = formin.lifespan + 1)
      ForminAction(Iterator(afterEating), -energyChange)
    }

    def moveForaminifera(foraminifera: Foraminifera, x: Int, y: Int): ForminAction = {
      val destinations = calculatePossibleDestinations(x, y, grid)
      val destination = destinations.filter(_._3 != Obstacle).nextOpt
      val afterMoving = foraminifera.copy(
        energy = foraminifera.energy - config.foraminiferaLifeActivityCost,
        lifespan = foraminifera.lifespan + 1
      )
      val (currentCell, moves) = destination match {
        case Opt((i, j, _)) =>
          val oldPlace = Iterator.empty
          val newPlace = Iterator(((i, j), afterMoving))
          (oldPlace, newPlace)
        case Opt.Empty =>
          (Iterator(afterMoving), Iterator.empty)
      }
      ForminAction(currentCell, moves = moves)
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
    //todo moves count metric
    println(foraminiferaDeaths, foraminiferaReproductionsCount, movesCount)
    (newGrid, metrics)
  }
}