package pl.edu.agh.formin.gui

import java.awt.Color
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, UIManager}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.avsystem.commons.SharedExtensions._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.formin.WorkerActor._
import pl.edu.agh.formin.algorithm.Metrics
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model.Grid.CellArray
import pl.edu.agh.formin.model._

import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing.Table.AbstractRenderer
import scala.swing._
import scala.util.Try

class GuiActor private(workers: Vector[ActorRef],
                       selected: ActorRef,
                       guiType: Either[GuiType.Basic.type, GuiType.Signal.type]
                      )(implicit config: ForminConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(config.gridSize, guiType)

  override def preStart: Unit = {
    //selected ! GetStatus
    //workers.foreach(_ ! RegisterMetrics)
    log.info("GUI started")
  }

  def started: Receive = {
    case IterationPartFinished(_, _, iteration, grid) if sender == selected =>
    //gui.setNewValues(iteration, grid)
    //selected ! GetStatus
    case IterationPartMetrics(workerId, iteration, metrics) =>
      gui.setWorkerIteration(workerId.value, iteration)
      if (sender == selected) gui.updatePlot(iteration, metrics)
  }
}

object GuiActor {

  def props(workers: Vector[ActorRef], selected: ActorRef, guiType: Either[GuiType.Basic.type, GuiType.Signal.type])
           (implicit config: ForminConfig): Props = {
    Props(new GuiActor(workers, selected, guiType))
  }

}

private[gui] class GuiGrid(dimension: Int, guiType: Either[GuiType.Basic.type, GuiType.Signal.type])
                          (implicit config: ForminConfig) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgcolor = new Color(220, 220, 220)
  private val cellView = guiType match {
    case Left(GuiType.Basic) => new ParticleCanvas(dimension, config.guiCellSize)
    case Right(GuiType.Signal) => new SignalTable(dimension)
  }

  private val chartPanel = new BorderPanel {
    background = bgcolor
  }
  private val chartPage = new Page("Plot", chartPanel)
  private val workersView = new Table(Array.tabulate(config.workersRoot * config.workersRoot)(id =>
    Array[Any](id + 1, 0)), Seq("Worker", "Iteration")
  )
  private val workersPanel = new BorderPanel {
    background = bgcolor
    layout(new ScrollPane(workersView)) = Center
  }

  def top = new MainFrame {
    title = "Formin model"
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
        pages += new Page("Workers", workersPanel)
      }

      layout(contentPane) = Center
    }

    contents = mainPanel
  }

  def setNewValues(iteration: Long, grid: Grid): Unit = {
    cellView.set(grid.cells.transpose)
  }

  def setWorkerIteration(workerId: Int, iteration: Long): Unit = {
    workersView.update(workerId - 1, 1, iteration)
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
          case AlgaeCell(_, _) => algaeColor
          case ForaminiferaCell(energy, _, _) =>
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
        case AlgaeCell(_,_) => algaeColor
        case ForaminiferaCell(_, _, _) => forminColor
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

  private val dataset = new XYSeriesCollection()
  private val foraminiferaXYSeries = new XYSeries("Foraminifera").setup(_.setMaximumItemCount(GuiGrid.MaximumPlotSize))
  private val algaeXYSeries = new XYSeries("Algae").setup(_.setMaximumItemCount(GuiGrid.MaximumPlotSize))
  dataset.addSeries(foraminiferaXYSeries)
  dataset.addSeries(algaeXYSeries)
  private val chart = ChartFactory.createXYLineChart(
    "Foraminifera and algae population size per iteration", "Iteration", "Population size", dataset, PlotOrientation.VERTICAL, true, true, false
  )
  private val panel = new ChartPanel(chart)
  chartPanel.layout(swing.Component.wrap(panel)) = Center

  def updatePlot(iteration: Long, metrics: Metrics): Unit = {
    foraminiferaXYSeries.add(iteration, metrics.foraminiferaCount)
    algaeXYSeries.add(iteration, metrics.algaeCount)
  }

  main(Array.empty)

}

object GuiGrid {
  final val MaximumPlotSize = 400
}

