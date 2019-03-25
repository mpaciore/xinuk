package pl.edu.agh.formin

import org.scalatest.{BeforeAndAfter, FlatSpecLike, Matchers}
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.xinuk.config.GuiType
import pl.edu.agh.xinuk.model._

class GridTest extends FlatSpecLike with Matchers with BeforeAndAfter {

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
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 1,
    iterationsNumber = 1000,
    isSupervisor = true,
    1
  )

  private var grid: Grid = _

  before {
    grid = Grid.empty(Set.empty)
  }

  "A Grid" should "have obstacles around" in {
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x == 0 || x == config.gridSize - 1 && y == 0 || y == config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe Obstacle
    }
  }

  it should "have empty cells inside" in {
    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && x != config.gridSize - 1 && y != 0 && y != config.gridSize - 1
    } {
      grid.cells(x)(y) shouldBe an[EmptyCell]
    }
  }

  it should "propagate signal correctly for one foraminifera cell" in {
    grid.cells(2)(2) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(3)(2) = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 3, 2)
    grid.cells(3)(1) = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 3, 1)

    grid.cells(2)(2).smell(0)(0).value shouldBe -1
    grid.cells(2)(2).smell(0)(1).value shouldBe -1
    grid.cells(2)(2).smell(0)(2).value shouldBe -1

    grid.cells(3)(2).smell(0)(1).value shouldBe -1.5

    grid.cells(3)(1).smell(0)(2).value shouldBe -0.5
  }

  it should "propagate signal correctly for one algae cell" in {
    grid.cells(2)(2) = AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
    grid.cells(3)(2) = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 3, 2)
    grid.cells(3)(1) = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 3, 1)

    grid.cells(2)(2).smell(0)(0).value shouldBe 1
    grid.cells(2)(2).smell(0)(1).value shouldBe 1
    grid.cells(2)(2).smell(0)(2).value shouldBe 1

    grid.cells(3)(2).smell(0)(1).value shouldBe 1.5

    grid.cells(3)(1).smell(0)(2).value shouldBe 0.5
  }

  it should "propagate signal correctly between algae and foraminifera cells" in {
    grid.cells(2)(2) = AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
    grid.cells(3)(2) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    val gridCellWithAlgaeAfterSignalPropagation = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 2, 2)
    val gridCellWithForaminiferaAfterSignalPropagation = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 3, 2)
    grid.cells(2)(2) = gridCellWithAlgaeAfterSignalPropagation
    grid.cells(3)(2) = gridCellWithForaminiferaAfterSignalPropagation

    grid.cells(2)(2).smell(2)(1).value shouldBe -0.5
    grid.cells(2)(2).smell(2)(0).value shouldBe 1
    grid.cells(2)(2).smell(2)(2).value shouldBe 1
    grid.cells(2)(2).smell(0)(0).value shouldBe 1
    grid.cells(2)(2).smell(0)(1).value shouldBe 1
    grid.cells(2)(2).smell(0)(2).value shouldBe 1

    grid.cells(3)(2).smell(0)(0).value shouldBe -1
    grid.cells(3)(2).smell(0)(1).value shouldBe 0.5
    grid.cells(3)(2).smell(0)(2).value shouldBe -1
    grid.cells(3)(2).smell(2)(0).value shouldBe -1
    grid.cells(3)(2).smell(2)(1).value shouldBe -1
    grid.cells(3)(2).smell(2)(2).value shouldBe -1
  }

  it should "not propagate signal on obstacle cell" in {
    grid.cells(3)(2) = ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(4)(2) = grid.propagatedSignal(DefaultSmellPropagation.calculateSmellAddendsStandard, 4, 2)
    grid.cells(3)(2).smell(1)(1).value shouldBe -1
    grid.cells(4)(2).smell(0)(1).value shouldBe 0
  }

  it should "calculate neighbour cells correctly for middle one" in {
    val gridNeighbourCellCoordinates = Grid.neighbourCellCoordinates(2, 2)
    gridNeighbourCellCoordinates.size shouldBe 8
    gridNeighbourCellCoordinates.head shouldBe(1, 1)
    gridNeighbourCellCoordinates(gridNeighbourCellCoordinates.size - 1) shouldBe(3, 3)
  }

  it should "calculate neighbour cells correctly for border one" in {
    val gridNeighbourCellCoordinates = Grid.neighbourCellCoordinates(0, 2)
    gridNeighbourCellCoordinates.size shouldBe 8
    gridNeighbourCellCoordinates.head shouldBe(-1, 1)
    gridNeighbourCellCoordinates(gridNeighbourCellCoordinates.size - 1) shouldBe(1, 3)
  }

  it should "make correct cell transformations from empty cell" in {
    val emptyCell: EmptyCell = EmptyCell.Instance
    val emptyCellWithForaminiferaInstantiated: GridPart =
      ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    val emptyCellWithAlgaeInstantiated: GridPart = AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
    val emptyCellWithSmell: EmptyCell =
      emptyCell.withSmell(emptyCellWithForaminiferaInstantiated.smell)
    val emptyCellWithForaminifera: ForaminiferaCell =
      ForaminiferaAccessible.unapply(emptyCell).withForaminifera(Energy(20), 3)
    val emptyCellWithAlgae: AlgaeCell = AlgaeAccessible.unapply(emptyCell).withAlgae(6)

    emptyCellWithSmell.smell shouldBe emptyCellWithForaminiferaInstantiated.smell

    emptyCellWithForaminifera.smell shouldBe emptyCellWithForaminiferaInstantiated.smell
    emptyCellWithForaminifera.energy shouldBe Energy(20)
    emptyCellWithForaminifera.lifespan shouldBe 3

    emptyCellWithAlgae.smell shouldBe emptyCellWithAlgaeInstantiated.smell
    emptyCellWithAlgae.lifespan shouldBe 6
  }

  it should "make correct cell transformations from foraminifera cell" in {
    val foraminiferaCell: ForaminiferaCell =
      ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    val emptyCellWithAlgaeInstantiated: GridPart = AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
    val foraminiferaCellWithSmell: ForaminiferaCell =
      foraminiferaCell.withSmell(emptyCellWithAlgaeInstantiated.smell)

    foraminiferaCellWithSmell.smell shouldBe emptyCellWithAlgaeInstantiated.smell
    foraminiferaCellWithSmell.energy shouldBe config.foraminiferaStartEnergy
    foraminiferaCellWithSmell.lifespan shouldBe 0
  }

  it should "make correct cell transformations from algae cell" in {
    val algaeCell: AlgaeCell = AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
    val emptyCellWithForaminiferaInstantiated: GridPart =
      ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
    val emptyCell: EmptyCell = EmptyCell.Instance
    val algaeCellWithSmell: AlgaeCell =
      algaeCell.withSmell(emptyCellWithForaminiferaInstantiated.smell)
    val algaeCellWithForaminifera: ForaminiferaCell =
      ForaminiferaAccessible.unapply(algaeCell).withForaminifera(Energy(20), 3)

    algaeCellWithSmell.smell shouldBe emptyCellWithForaminiferaInstantiated.smell
    algaeCellWithSmell.lifespan shouldBe 0

    algaeCellWithForaminifera.smell shouldBe emptyCell.smell
    algaeCellWithForaminifera.energy shouldBe Energy(20.4)
    algaeCellWithForaminifera.lifespan shouldBe 3
  }
}
