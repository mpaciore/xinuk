package pl.edu.agh.formin.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}
import javax.swing.{BorderFactory, ImageIcon, UIManager}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.formin.WorkerActor.{IterationPartFinished, Register}
import pl.edu.agh.formin.WorkerId
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model.Grid.CellArray
import pl.edu.agh.formin.model._

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing.Table.AbstractRenderer
import scala.swing._
import scala.util.Try

class GuiActor private(
                        workers: TreeMap[WorkerId, ActorRef],
                        guiType: Either[GuiType.Basic.type, GuiType.Signal.type]
                      )(implicit config: ForminConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(config.gridSize, guiType)

  override def preStart: Unit = {
    workers.values.headOption.foreach(_ ! Register)
    log.info("GUI started")
  }

  def started: Receive = {
    //todo fix counts
    case IterationPartFinished(iteration, status) =>
      gui.setNewValues(status.grid, iteration)
  }
}

object GuiActor {

  def props(workers: TreeMap[WorkerId, ActorRef], guiType: Either[GuiType.Basic.type, GuiType.Signal.type])(implicit config: ForminConfig): Props = {
    Props(new GuiActor(workers, guiType))
  }

}

private[gui] class GuiGrid(dimension: Int, guiType: Either[GuiType.Basic.type, GuiType.Signal.type])(implicit config: ForminConfig)
  extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgcolor = new Color(220, 220, 220)
  private val iterations = mutable.ListBuffer.empty[Long]
  private val forminSeries = mutable.ListBuffer.empty[Long]
  private val algaeSeries = mutable.ListBuffer.empty[Long]
  private val cellView = guiType match {
    case Left(GuiType.Basic) => new ParticleCanvas(dimension, config.guiCellSize)
    case Right(GuiType.Signal) => new SignalTable(dimension)
  }

  private val chartPanel = new BorderPanel {
    background = bgcolor
  }
  private val chartPage = new Page("Plot", chartPanel)
  private val iterationLabel = new Label {
    private var _iteration: Long = _

    def iteration: Long = _iteration

    def setIteration(iteration: Long): Unit = {
      _iteration = iteration
      text = s"Iteration: $iteration"
    }

    border = BorderFactory.createEmptyBorder(50, 20, 50, 20)
  }

  def top = new MainFrame {
    title = "Formin model"
    minimumSize = new Dimension(1200, 800)
    background = bgcolor

    val mainPanel = new BorderPanel {

      val cellPanel = new BorderPanel {
        val view = new BorderPanel {
          background = bgcolor
          layout(cellView) = Center
        }
        background = bgcolor
        layout(view) = Center
      }


      val contentPane = new TabbedPane {
        pages += new Page("Cells", cellPanel)
        pages += chartPage
      }

      val statusPanel = new BorderPanel {
        layout(iterationLabel) = Center
      }

      layout(contentPane) = Center
      layout(statusPanel) = East
    }

    contents = mainPanel
  }

  def setNewValues(newGrid: Grid, iteration: Long): Unit = {
    cellView.set(newGrid.cells)
    updateForminAlgaeCount(newGrid.cells, iteration)
    plot()
    iterationLabel.setIteration(iteration)
  }

  def updateForminAlgaeCount(cells: CellArray, iteration: Long): Unit = {
    var forminCounter = 0
    var algaeCounter = 0

    for {
      x <- cells.indices
      y <- cells.indices
    } {
      cells(x)(y) match {
        case AlgaeCell(_) => algaeCounter += 1
        case ForaminiferaCell(_, _) => forminCounter += 1
        case _ =>
      }
    }
    iterations += iteration
    forminSeries += forminCounter.toLong
    algaeSeries += algaeCounter.toLong

  }

  sealed trait CellArraySettable extends Component {
    def set(cells: CellArray): Unit
  }

  private class SignalTable(dimension: Int) extends Table(Cell.Size * dimension, Cell.Size * dimension) with CellArraySettable {
    private val algaeColor = new swing.Color(9, 108, 16)
    private val forminColor = new swing.Color(81, 71, 8)
    private val obstacleColor = new swing.Color(0, 0, 0)
    private val bufferColor = new swing.Color(163, 163, 194)
    private val emptyColor = new swing.Color(255, 255, 255)
    private val renderer = new AbstractRenderer(new CellLabel) {
      override def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Any, row: Int, column: Int): Unit = {
        component.prepare(row, column)
      }
    }

    var cells: CellArray = _

    class CellLabel extends Label {

      def prepare(row: Int, column: Int): Unit = {
        text = cells(column / Cell.Size)(row / Cell.Size).smell(column % Cell.Size)(row % Cell.Size).value.toString
        background = cells(column / Cell.Size)(row / Cell.Size) match {
          case AlgaeCell(_) => algaeColor
          case ForaminiferaCell(energy, _) =>
            if (row % Cell.Size == 1 && column % Cell.Size == 1) {
              text = energy.value.toString
              new swing.Color(255, (0 + 255 * energy.value / 2).toInt, (0 + 255 * energy.value / 2).toInt)
            } else {
              forminColor
            }
          case Obstacle => obstacleColor
          case BufferCell(_) => bufferColor
          case EmptyCell(_) => emptyColor
        }
      }
    }

    def set(cells: CellArray): Unit = {
      this.cells = cells
      this.repaint()
    }

    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int): Component = {
      renderer.componentFor(this, isSelected = false, hasFocus = false, 0, row, col)
    }

  }

  private class ParticleCanvas(dimension: Int, guiCellSize: Int) extends Label with CellArraySettable {
    private val algaeColor = new swing.Color(9, 108, 16).getRGB
    private val forminColor = new swing.Color(81, 71, 8).getRGB
    private val obstacleColor = new swing.Color(0, 0, 0).getRGB
    private val bufferColor = new swing.Color(163, 163, 194).getRGB
    private val emptyColor = new swing.Color(255, 255, 255).getRGB
    private val img = new BufferedImage(dimension * guiCellSize, dimension * guiCellSize, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cells: CellArray): Unit = {
      val rgbArray = cells.map(_.map {
        case AlgaeCell(_) => algaeColor
        case ForaminiferaCell(_, _) => forminColor
        case Obstacle => obstacleColor
        case BufferCell(_) => bufferColor
        case EmptyCell(_) => emptyColor
      })

      for {
        x <- cells.indices
        y <- cells.indices
      } {
        val startX = x * guiCellSize
        val startY = y * guiCellSize
        img.setRGB(startX, startY, guiCellSize, guiCellSize, Array.fill(guiCellSize * guiCellSize)(rgbArray(x)(y)), 0, guiCellSize)
      }
      this.repaint()
    }
  }

  def plot(): Unit = {
    val dataset = new XYSeriesCollection()
    val foraminiferaXYSeries = new XYSeries("Foraminifera")
    val algaeXYSeries = new XYSeries("Algae")
    for (i <- iterations.indices) {
      foraminiferaXYSeries.add(iterations(i), forminSeries(i))
      algaeXYSeries.add(iterations(i), algaeSeries(i))
    }
    dataset.addSeries(foraminiferaXYSeries)
    dataset.addSeries(algaeXYSeries)
    val chart = ChartFactory.createXYLineChart(
      "Foraminifera and algae population size per iteration", "Iteration", "Population size", dataset, PlotOrientation.VERTICAL, true, true, false
    )
    val panel = new ChartPanel(chart)
    chartPanel.layout(swing.Component.wrap(panel)) = Center
  }

  main(Array.empty)

}

