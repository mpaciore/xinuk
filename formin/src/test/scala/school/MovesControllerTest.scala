package pl.edu.agh.school

import com.avsystem.commons.misc.Opt
import org.scalatest.{BeforeAndAfter, FlatSpecLike, Matchers}
import pl.edu.agh.school.algorithm.ForminMovesController
import pl.edu.agh.school.config.ForminConfig
import pl.edu.agh.school.model.ForaminiferaAccessible
import pl.edu.agh.xinuk.config.GuiType
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet

class MovesControllerTest extends FlatSpecLike with Matchers with BeforeAndAfter {
  implicit val config: ForminConfig = ForminConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppressionFactor = 0.5,
    signalAttenuationFactor = 1,
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = List(Signal(-1), Signal(0)),
    foraminiferaPursuedSignalIndex = 0,
    algaeInitialSignal = List(Signal(1), Signal(0)),
    algaePursuedSignalIndex = 0,
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 1,
    iterationsNumber = 1000,
    isSupervisor = true,
    shardingMod = 1
  )

  private var grid: Grid = _

  before {
    grid = Grid.empty(Set.empty)
  }

  "A calculatePossibleDestinations method" should "return correct possible destination cells" in {
    val movesController: ForminMovesController = new ForminMovesController(TreeSet.empty)
    val cell1 = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(2) = cell1
    grid.cells(3)(2) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(3) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)

    grid.cells(2)(2).smell(0)(0) = SignalVector(Array(Signal(20), Signal(0)))
    grid.cells(2)(2).smell(0)(2) = SignalVector(Array(Signal(15), Signal(0)))
    grid.cells(2)(2).smell(1)(0) = SignalVector(Array(Signal(-5), Signal(0)))
    grid.cells(2)(2).smell(2)(2) = SignalVector(Array(Signal(-666), Signal(0)))

    val (x, y, destination) = movesController.calculatePossibleDestinations(cell1, 2, 2, grid).next()

    x shouldBe 1
    y shouldBe 1
    destination shouldBe grid.cells(1)(1)
  }

  "A selectDestinationCell method" should "return right destination cell for first one correct" in {
    val movesController = new ForminMovesController(TreeSet.empty)
    val cell1 = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(2) = cell1
    grid.cells(3)(2) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(3) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)

    grid.cells(2)(2).smell(0)(0) = SignalVector(Array(Signal(20), Signal(0)))
    grid.cells(2)(2).smell(0)(2) = SignalVector(Array(Signal(15), Signal(0)))
    grid.cells(2)(2).smell(1)(0) = SignalVector(Array(Signal(-5), Signal(0)))
    grid.cells(2)(2).smell(2)(2) = SignalVector(Array(Signal(-666), Signal(0)))

    val destinations = movesController.calculatePossibleDestinations(cell1, 2, 2, grid)
    val destination = movesController.selectDestinationCell(destinations,grid)

    destination shouldBe Opt((1, 1, grid.cells(1)(1)))
  }

  it should "return right destination cell for not first one correct" in {
    val movesController = new ForminMovesController(TreeSet.empty)
    val cell1 = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(2) = cell1
    grid.cells(3)(2) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(3) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)

    grid.cells(2)(2).smell(0)(0) = SignalVector(Array(Signal(4), Signal(0)))
    grid.cells(2)(2).smell(0)(2) = SignalVector(Array(Signal(15), Signal(0)))
    grid.cells(2)(2).smell(1)(0) = SignalVector(Array(Signal(-5), Signal(0)))
    grid.cells(2)(2).smell(2)(2) = SignalVector(Array(Signal(-666), Signal(0)))

    val destinations = movesController.calculatePossibleDestinations(cell1, 2, 2, grid)
    val destination = movesController.selectDestinationCell(destinations,grid)

    destination shouldBe Opt((1, 3, grid.cells(3)(1)))
  }

}
