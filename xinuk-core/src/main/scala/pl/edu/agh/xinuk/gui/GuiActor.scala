package pl.edu.agh.xinuk.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import javax.swing.{ImageIcon, UIManager}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.gui.GuiActor.GridInfo
import pl.edu.agh.xinuk.model.Grid.CellArray
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.Metrics
import pl.edu.agh.xinuk.simulation.WorkerActor._

import scala.collection.mutable
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.util.{Random, Try}

class GuiActor private(
                        worker: ActorRef, workerId: WorkerId, cellToColor: PartialFunction[GridPart, Color]
                      )(implicit config: XinukConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(cellToColor, workerId)

  override def preStart: Unit = {
    worker ! SubscribeGridInfo(workerId)
    log.info("GUI started")
  }

  def started: Receive = {
    case GridInfo(iteration, grid, metrics) =>
      gui.setNewValues(iteration, grid)
      gui.updatePlot(iteration, metrics)
  }
}

object GuiActor {

  final case class GridInfo private(iteration: Long, grid: Grid, metrics: Metrics)

  def props(worker: ActorRef, workerId: WorkerId, cellToColor: PartialFunction[GridPart, Color])
           (implicit config: XinukConfig): Props = {
    Props(new GuiActor(worker, workerId, cellToColor))
  }

}

private[gui] class GuiGrid(cellToColor: PartialFunction[GridPart, Color], workerId: WorkerId)(implicit config: XinukConfig) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgcolor = new Color(0, 0, 0)
  private val cellView = new ParticleCanvas(config.gridSize, config.guiCellSize)
  private val chartPanel = new BorderPanel {
    background = bgcolor
  }
  private val chartPage = new Page("Plot", chartPanel)
  private val (alignedLocation, alignedSize) = alignFrame()

  def top = new MainFrame {
    title = "Xinuk"
    background = bgcolor
    location = alignedLocation
    preferredSize = alignedSize

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

      layout(contentPane) = Center
    }

    contents = mainPanel
  }

  private def alignFrame(): (Point, Dimension) = {
    val xOffset = 100
    val yOffset = 100
    val width = config.gridSize * config.guiCellSize + 25
    val height = config.gridSize * config.guiCellSize + 75
    val xPos = (workerId.value - 1) % config.workersRoot
    val yPos = (workerId.value - 1) / config.workersRoot
    (new Point(xOffset + xPos * width, yOffset + yPos * height), new Dimension(width, height))
  }

  def setNewValues(iteration: Long, grid: Grid): Unit = {
    cellView.set(grid.cells.transpose)
  }

  private class ParticleCanvas(dimension: Int, guiCellSize: Int) extends Label {

    private val obstacleColor = new swing.Color(0, 0, 0)
    private val bufferColor = new swing.Color(163, 163, 194)
    private val emptyColor = new swing.Color(255, 255, 255)
    private val img = new BufferedImage(dimension * guiCellSize, dimension * guiCellSize, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    private val classToColor = mutable.Map[Class[_], Color](
      Obstacle.getClass -> obstacleColor,
      classOf[BufferCell] -> bufferColor,
      classOf[EmptyCell] -> emptyColor
    )

    def set(cells: CellArray): Unit = {
      def generateColor(cell: GridPart): Color = {
        val random = new Random(cell.getClass.hashCode())
        val hue = random.nextFloat()
        val saturation = 1.0f
        val luminance = 0.6f
        Color.getHSBColor(hue, saturation, luminance)
      }

      val rgbArray: Array[Array[Color]] = cells.map(_.map(cell =>
        cellToColor.applyOrElse(cell, (_: GridPart) => classToColor.getOrElseUpdate(cell.getClass, generateColor(cell)))
      ))

      for {
        x <- cells.indices
        y <- cells.indices
      } {
        val startX = x * guiCellSize
        val startY = y * guiCellSize
        img.setRGB(startX, startY, guiCellSize, guiCellSize, Array.fill(guiCellSize * guiCellSize)(rgbArray(x)(y).getRGB), 0, guiCellSize)
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
      nameToSeries.getOrElseUpdate(name, createSeries(name)).add(iteration, value)
    }
  }

  main(Array.empty)

}

object GuiGrid {
  final val MaximumPlotSize = 400
}

