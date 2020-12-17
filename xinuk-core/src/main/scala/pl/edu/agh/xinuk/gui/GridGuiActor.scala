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
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldShard}
import pl.edu.agh.xinuk.simulation.WorkerActor.{GridInfo, MsgWrapper, SubscribeGridInfo}

import scala.collection.mutable
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.util.{Random, Try}

class GridGuiActor private(worker: ActorRef,
                           simulationId: String,
                           workerId: WorkerId,
                           bounds: GridWorldShard.Bounds)
                          (implicit config: XinukConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(bounds, workerId)

  override def preStart(): Unit = {
    worker ! MsgWrapper(workerId, SubscribeGridInfo())
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
    gui.quit()
  }

  def started: Receive = {
    case GridInfo(iteration, cellColors, metrics) =>
      gui.setNewValues(cellColors)
      gui.updatePlot(iteration, metrics)
  }
}

object GridGuiActor {
  def props(worker: ActorRef, simulationId: String, workerId: WorkerId, bounds: GridWorldShard.Bounds)
           (implicit config: XinukConfig): Props = {
    Props(new GridGuiActor(worker, simulationId, workerId, bounds))
  }
}

private[gui] class GuiGrid(bounds: GridWorldShard.Bounds, workerId: WorkerId)
                          (implicit config: XinukConfig) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgColor = new Color(220, 220, 220)
  private val cellView = new ParticleCanvas(bounds.xMin, bounds.yMin, bounds.xSize, bounds.ySize, config.guiCellSize)
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
    val xPos = (workerId.value - 1) / config.workersY
    val yPos = (workerId.value - 1) % config.workersY

    val xGlobalOffset = 100
    val yGlobalOffset = 0

    val xWindowAdjustment = 24
    val yWindowAdjustment = 70

    val xLocalOffset = bounds.xMin * config.guiCellSize + xPos * xWindowAdjustment
    val yLocalOffset = bounds.yMin * config.guiCellSize + yPos * yWindowAdjustment

    val width = bounds.xSize * config.guiCellSize + xWindowAdjustment
    val height = bounds.ySize * config.guiCellSize + yWindowAdjustment

    val location = new Point(xGlobalOffset + xLocalOffset, yGlobalOffset + yLocalOffset)
    val size = new Dimension(width, height)
    (location, size)
  }

  def setNewValues(cellColors: Map[CellId, Color]): Unit = {
    cellView.set(cellColors)
  }

  private class ParticleCanvas(xOffset: Int, yOffset: Int, xSize: Int, ySize: Int, guiCellSize: Int) extends Label {
    private val img = new BufferedImage(xSize * guiCellSize, ySize * guiCellSize, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cellColors: Map[CellId, Color]): Unit = {
      cellColors.foreach {
        case (GridCellId(x, y), color) =>
          val startX = (x - xOffset) * guiCellSize
          val startY = (y - yOffset) * guiCellSize
          img.setRGB(startX, startY, guiCellSize, guiCellSize, Array.fill(guiCellSize * guiCellSize)(color.getRGB), 0, guiCellSize)
        case _ =>
      }
      this.repaint()
    }
  }

  private val nameToSeries = mutable.Map.empty[String, XYSeries]
  private val dataset = new XYSeriesCollection()
  private val chart = ChartFactory.createXYLineChart(
    "Iteration metrics chart", "X", "Y", dataset, PlotOrientation.VERTICAL, true, true, false
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

