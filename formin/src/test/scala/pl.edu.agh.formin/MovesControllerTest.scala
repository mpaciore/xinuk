package pl.edu.agh.formin
import com.avsystem.commons.misc.Opt
import org.scalatest.{BeforeAndAfter, FlatSpecLike, Matchers}
import org.slf4j.LoggerFactory
import pl.edu.agh.formin.algorithm.MovesController
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model._

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
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 1,
    iterationsNumber = 1000,
    isSupervisor = true
  )

  private var grid: Grid = _

  before {
    grid = Grid.empty(Set.empty)
  }

  "A calculatePossibleDestinations method" should "return correct possible destination cells" in {
    val movesController = new MovesController(TreeSet.empty, LoggerFactory.getLogger(""))
    val cell1 = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(2) = cell1
    grid.cells(3)(2) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(3) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)

    grid.cells(2)(2).smell(0)(0) = Signal(20)
    grid.cells(2)(2).smell(0)(2) = Signal(15)
    grid.cells(2)(2).smell(1)(0) = Signal(-5)
    grid.cells(2)(2).smell(2)(2) = Signal(-666)

    val (x, y, destination) = movesController.calculatePossibleDestinations(cell1, 2, 2, grid).next()

    x shouldBe 1
    y shouldBe 1
    destination shouldBe grid.cells(1)(1)
  }

  "A selectDestinationCell method" should "return right destination cell for first one correct" in {
    val movesController = new MovesController(TreeSet.empty, LoggerFactory.getLogger(""))
    val cell1 = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(2) = cell1
    grid.cells(3)(2) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(3) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)

    grid.cells(2)(2).smell(0)(0) = Signal(20)
    grid.cells(2)(2).smell(0)(2) = Signal(15)
    grid.cells(2)(2).smell(1)(0) = Signal(-5)
    grid.cells(2)(2).smell(2)(2) = Signal(-666)

    val destinations = movesController.calculatePossibleDestinations(cell1, 2, 2, grid)
    val destination = movesController.selectDestinationCell(destinations,grid)

    destination shouldBe Opt((1, 1, grid.cells(1)(1)))
  }

  it should "return right destination cell for not first one correct" in {
    val movesController = new MovesController(TreeSet.empty, LoggerFactory.getLogger(""))
    val cell1 = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(2) = cell1
    grid.cells(3)(2) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(2)(3) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)

    grid.cells(2)(2).smell(0)(0) = Signal(4)
    grid.cells(2)(2).smell(0)(2) = Signal(15)
    grid.cells(2)(2).smell(1)(0) = Signal(-5)
    grid.cells(2)(2).smell(2)(2) = Signal(-666)

    val destinations = movesController.calculatePossibleDestinations(cell1, 2, 2, grid)
    val destination = movesController.selectDestinationCell(destinations,grid)

    destination shouldBe Opt((1, 3, grid.cells(3)(1)))
  }

}
