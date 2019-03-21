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
      foraminiferaTotalLifespan = 0,
      foraminiferaMoves = 0
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
      def updated(cell: FortwistCell): FortwistCell = {
        val afterOp = op(cell)
        val smellAdjustment = (config.foraminiferaInitialSignal * afterOp.foraminiferas.size) +
          (config.algaeSignalMultiplier * afterOp.algae.value)
        afterOp.copy(smell = afterOp.smell + smellAdjustment)
      }

      newGrid.cells(x)(y) = newGrid.cells(x)(y) match {
        case cell: FortwistCell => updated(cell)
        case BufferCell(cell: FortwistCell) => BufferCell(updated(cell))
      }
    }

    def makeMove(x: Int, y: Int): Unit = {
      this.grid.cells(x)(y) match {
        case Obstacle =>
        case BufferCell(FortwistCell(smell, _, _)) =>
          update(x, y)(cell => cell.copy(smell = cell.smell + smell))
        case FortwistCell(smell, foraminiferas, algaeEnergy) => {
          val (newForaminiferas: Iterator[Foraminifera], moves: BMap[(Int, Int), Stream[Foraminifera]], newAlgaeEnergy: Energy) =
            foraminiferas.foldLeft(
              (Iterator[Foraminifera](), MMap.empty[(Int, Int), Stream[Foraminifera]].withDefaultValue(Stream.empty), algaeEnergy)
            ) { case ((currentCellResult, pendingMoves, runningAlgaeEnergy), formin) =>
              val action = if (formin.energy > config.foraminiferaReproductionThreshold) {
                reproduceForaminifera(formin)
              } else if (runningAlgaeEnergy > config.algaeEnergeticCapacity) {
                eatAlgae(formin)
              } else if (formin.energy < config.foraminiferaLifeActivityCost) {
                killForaminifera(formin)
              } else {
                moveForaminifera(formin, x, y, pendingMoves)
              }
              action.moves.foreach { case ((x, y), movingFormin) => pendingMoves((x, y)) = pendingMoves((x, y)) :+ movingFormin }
              (currentCellResult ++ action.currentCellResult, pendingMoves, runningAlgaeEnergy + action.algaeEnergyDiff)
            }
          import Cell._
          update(x, y)(cell => cell.copy(
            smell = cell.smell + smell,
            foraminiferas = cell.foraminiferas ++ newForaminiferas,
            algae = Energy(math.min(1.0, newAlgaeEnergy.value + (math.sqrt(newAlgaeEnergy.value) * config.algaeRegenerationRate)))
          ))
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
                                   moves: Iterator[((Int, Int), Foraminifera)] = Iterator.empty
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

    def moveForaminifera(foraminifera: Foraminifera, x: Int, y: Int, moves: BMap[(Int, Int), Stream[Foraminifera]]): ForminAction = {
      def calculatePossibleDestinations(x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
        val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
        Grid.SubcellCoordinates
          .map { case (i, j) => grid.cells(x)(y).smell(i)(j) + moves.get((x, y)).map(formins => config.foraminiferaInitialSignal * formins.size).getOrElse(Signal.Zero) }
          .zipWithIndex
          .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
          .iterator
          .map { case (_, idx) =>
            val (i, j) = neighbourCellCoordinates(idx)
            (i, j, grid.cells(i)(j))
          }
      }

      val destinations = calculatePossibleDestinations(x, y, grid)
      val destination = destinations.filter(_._3 != Obstacle).nextOpt
      val afterMoving = foraminifera.copy(
        energy = foraminifera.energy - config.foraminiferaLifeActivityCost,
        lifespan = foraminifera.lifespan + 1
      )
      val (currentCell, outgoingMoves) = destination match {
        case Opt((i, j, _)) =>
          val oldPlace = Iterator.empty
          val newPlace = Iterator(((i, j), afterMoving))
          (oldPlace, newPlace)
        case Opt.Empty =>
          (Iterator(afterMoving), Iterator.empty)
      }
      ForminAction(currentCell, moves = outgoingMoves)
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      this.grid.cells(x)(y) match {
        case FortwistCell(_, foraminiferas, algae) =>
          foraminiferaTotalEnergy += foraminiferas.iterator.map(_.energy.value).sum
          foraminiferaCount += foraminiferas.size
          algaeCount += algae.value
        case BufferCell(FortwistCell(_, foraminiferas, algae)) =>
          foraminiferaTotalEnergy += foraminiferas.iterator.map(_.energy.value).sum
          foraminiferaCount += foraminiferas.size
          algaeCount += algae.value
        case _ =>
      }
    }

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } makeMove(x, y)

    val metrics = FortwistMetrics(
      foraminiferaCount = foraminiferaCount,
      algaeCount = algaeCount,
      foraminiferaDeaths = foraminiferaDeaths,
      foraminiferaTotalEnergy = foraminiferaTotalEnergy,
      foraminiferaReproductionsCount = foraminiferaReproductionsCount,
      consumedAlgaeCount = consumedAlgaeCount,
      foraminiferaTotalLifespan = foraminiferaTotalLifespan,
      foraminiferaMoves = movesCount
    )
    (newGrid, metrics)
  }
}