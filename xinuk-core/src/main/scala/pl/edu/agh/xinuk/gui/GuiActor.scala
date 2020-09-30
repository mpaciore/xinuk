package pl.edu.agh.xinuk.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import javax.swing.{ImageIcon, UIManager}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.xinuk.algorithm.Metrics
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.gui.GuiActor.GridInfo
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId
import pl.edu.agh.xinuk.simulation.WorkerActor.{MsgWrapper, SubscribeGridInfo}

import scala.collection.mutable
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.util.{Random, Try}

class GuiActor private(worker: ActorRef,
                       workerId: WorkerId,
                       worldSpan: ((Int, Int), (Int, Int)),
                       cellToColor: PartialFunction[CellState, Color])
                      (implicit config: XinukConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(worldSpan, cellToColor, workerId)

  override def preStart(): Unit = {
    worker ! MsgWrapper(workerId, SubscribeGridInfo())
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
    gui.quit()
  }

  def started: Receive = {
    case GridInfo(iteration, cells, metrics) =>
      gui.setNewValues(iteration, cells)
      gui.updatePlot(iteration, metrics)
  }
}

object GuiActor {

  final case class GridInfo private(iteration: Long, cells: Set[Cell], metrics: Metrics)

  def props(worker: ActorRef, workerId: WorkerId, worldSpan: ((Int, Int), (Int, Int)), cellToColor: PartialFunction[CellState, Color])
           (implicit config: XinukConfig): Props = {
    Props(new GuiActor(worker, workerId, worldSpan, cellToColor))
  }

}

private[gui] class GuiGrid(worldSpan: ((Int, Int), (Int, Int)), cellToColor: PartialFunction[CellState, Color], workerId: WorkerId)
                          (implicit config: XinukConfig) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val ((xOffset, yOffset), (xSize, ySize)) = worldSpan
  private val bgColor = new Color(220, 220, 220)
  private val cellView = new ParticleCanvas(xOffset, yOffset, xSize, ySize, config.guiCellSize)
  private val chartPanel = new BorderPanel {
    background = bgColor
  }
  private val chartPage = new Page("Plot", chartPanel)
  private val (alignedLocation, alignedSize) = alignFrame()

  def top: MainFrame = new MainFrame {
    title = s"Xinuk ${workerId.value}"
    background = bgColor
    location = alignedLocation
    preferredSize = alignedSize

    val mainPanel: BorderPanel = new BorderPanel {

      val cellPanel: BorderPanel = new BorderPanel {
        val view: BorderPanel = new BorderPanel {
          background = bgColor
          layout(cellView) = Center
        }
        background = bgColor
        layout(view) = Center
      }

      val contentPane: TabbedPane = new TabbedPane {
        pages += new Page("Cells", cellPanel)
        pages += chartPage
      }

      layout(contentPane) = Center
    }

    contents = mainPanel
  }

  private def alignFrame(): (Point, Dimension) = {
    val xPos = (workerId.value - 1) / config.workersRoot
    val yPos = (workerId.value - 1) % config.workersRoot

    val xGlobalOffset = 100
    val yGlobalOffset = 0

    val xWindowAdjustment = 24
    val yWindowAdjustment = 70

    val xLocalOffset = xOffset * config.guiCellSize + xPos * xWindowAdjustment
    val yLocalOffset = yOffset * config.guiCellSize + yPos * yWindowAdjustment

    val width = xSize * config.guiCellSize + xWindowAdjustment
    val height = ySize * config.guiCellSize + yWindowAdjustment

    val location = new Point(xGlobalOffset + xLocalOffset, yGlobalOffset + yLocalOffset)
    val size = new Dimension(width, height)
    (location, size)
  }

  def setNewValues(iteration: Long, cells: Set[Cell]): Unit = {
    cellView.set(cells)
  }

  private class ParticleCanvas(xOffset: Int, yOffset: Int, xSize: Int, ySize: Int, guiCellSize: Int) extends Label {

    private val obstacleColor = new swing.Color(0, 0, 0)
    private val emptyColor = new swing.Color(255, 255, 255)
    private val img = new BufferedImage(xSize * guiCellSize, ySize * guiCellSize, BufferedImage.TYPE_INT_ARGB)

    private def defaultColor: CellState => Color =
      state => state.contents match {
        case Obstacle => obstacleColor
        case Empty => emptyColor
        case other =>
          val random = new Random(other.getClass.hashCode())
          val hue = random.nextFloat()
          val saturation = 1.0f
          val luminance = 0.6f
          Color.getHSBColor(hue, saturation, luminance)
      }

    icon = new ImageIcon(img)

    def set(cells: Set[Cell]): Unit = {
      cells.foreach {
        case Cell(GridCellId(x, y), state) =>
          val startX = (x - xOffset) * guiCellSize
          val startY = (y - yOffset) * guiCellSize
          val color: Color = cellToColor.applyOrElse(state, defaultColor)
          img.setRGB(startX, startY, guiCellSize, guiCellSize, Array.fill(guiCellSize * guiCellSize)(color.getRGB), 0, guiCellSize)
        case _ =>
      }
      this.repaint()
    }
  }

  private val nameToSeries = mutable.Map.empty[String, XYSeries]
  private val dataset = new XYSeriesCollection()
  private val chart = ChartFactory.createXYLineChart(
    "Iteration metrics chart", "X", "Y size", dataset, PlotOrientation.VERTICAL, true, true, false
  )
  private val panel = new ChartPanel(chart)
  chartPanel.layout(swing.Component.wrap(panel)) = Center

  def updatePlot(iteration: Long, metrics: Metrics): Unit = {
    def createSeries(name: String): XYSeries = {
      val series = new XYSeries(name)
      series.setMaximumItemCount(GuiGrid.MaximumPlotSize)
      dataset.addSeries(series)
      series
    }

    metrics.series.foreach { case (name, value) =>
      nameToSeries.getOrElseUpdate(name, createSeries(name)).add(iteration.toDouble, value)
    }
  }

  main(Array.empty)

}

object GuiGrid {
  final val MaximumPlotSize = 400
}

