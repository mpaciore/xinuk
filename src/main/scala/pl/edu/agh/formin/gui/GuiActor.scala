package pl.edu.agh.formin.gui
import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}
import javax.swing.{ImageIcon, UIManager}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import pl.edu.agh.formin.SchedulerActor.Register
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.{IterationStatus, WorkerId}

import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing.Table.AbstractRenderer
import scala.swing._
import scala.util.Try

class GuiActor private(scheduler: ActorRef, worker: WorkerId)(implicit config: ForminConfig) extends Actor with ActorLogging {

  import GuiActor._

  override def receive: Receive = started

  private lazy val gui: GuiGrid = new GuiGrid(config.gridSize)

  override def preStart: Unit = {
    scheduler ! Register
    log.info("GUI started")
  }

  def started: Receive = {
    case NewIteration(state) =>
      state.getGridForWorker(worker) match {
        case Some(grid) =>
          gui.setNewValues(grid)
        case None =>
          log.error("Worker {} grid status unavailable", worker.value)
      }
  }
}

object GuiActor {

  case class NewIteration(state: IterationStatus) extends AnyVal

  def props(scheduler: ActorRef, worker: WorkerId)(implicit config: ForminConfig): Props = {
    Props(new GuiActor(scheduler, worker))
  }
}

private[gui] class GuiGrid(dimension: Int) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgcolor = new Color(220, 220, 220)
  private val pc = new ParticleCanvas(dimension)


  private val tb = new Table(dimension * 3, dimension * 3) {

    private val algaeColor = new swing.Color(9, 108, 16)
    private val forminColor = new swing.Color(81, 71, 8)
    private val obstacleColor = new swing.Color(0, 0, 0)
    private val emptyColor = new swing.Color(255, 255, 255)

    var renderer = new CellRenderer(new Array[Array[Cell]](dimension))

    class CellRenderer(cells: Array[Array[Cell]])  extends AbstractRenderer(new CellLabel) {
      override def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: Any, row: Int, column: Int): Unit = {
        component.prepare(row, column, cells)
      }
    }

    class CellLabel extends Label {
      def prepare(row : Int, column : Int, cells: Array[Array[Cell]]) {
        cells(column/3)(row/3) match {
          case AlgaeCell(_) => background = algaeColor
          case ForaminiferaCell(_, _) => background =  forminColor
          case Obstacle => background =  obstacleColor
          case EmptyCell(_) => background =  emptyColor
        }

        text = cells(column/3)(row/3).smell(column%3)(row%3).value.toString
      }
    }

    def set(cells: Array[Array[Cell]]): Unit = {
      renderer = new CellRenderer(cells)
    }

    override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int): Component = {
      renderer.componentFor(this,isSelected = false,hasFocus = false,0,row,col)
    }
  }


  def top = new MainFrame {
    title = "Formin model"
    minimumSize = new Dimension(1200, 800)
    background = bgcolor

    val canvas = new BorderPanel {
      background = bgcolor
      layout(pc) = Center
    }

    val table = new BorderPanel {
      background = bgcolor
      layout(tb) = Center
    }

    val simPanel = new BorderPanel {
      background = bgcolor
      layout(canvas) = Center
    }
    val sigPanel = new BorderPanel {
      background = bgcolor
      layout(table) = Center
    }
    contents = new TabbedPane {
      pages += new Page("Simulation", simPanel)
      pages += new Page("Signal", sigPanel)
    }
  }

  def setNewValues(newGrid: Grid): Unit = {
    pc.set(newGrid.cells)
    tb.set(newGrid.cells)
    tb.repaint()
    pc.repaint()
  }

  private class ParticleCanvas(dimension: Int) extends Label {
    private val factor = 40
    private val algaeColor = new swing.Color(9, 108, 16).getRGB
    private val forminColor = new swing.Color(81, 71, 8).getRGB
    private val obstacleColor = new swing.Color(0, 0, 0).getRGB
    private val emptyColor = new swing.Color(255, 255, 255).getRGB
    private val img = new BufferedImage(dimension * factor, dimension * factor, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cells: Array[Array[Cell]]): Unit = {
      val rgbArray = cells.map(_.map {
        case AlgaeCell(_) => algaeColor
        case ForaminiferaCell(_, _) => forminColor
        case Obstacle => obstacleColor
        case EmptyCell(_) => emptyColor
      })

      for {
        x <- cells.indices
        y <- cells.indices
      } {
        val startX = x * factor
        val startY = y * factor
        //todo optimize (array gen)
        img.setRGB(startX, startY, factor, factor, Array.fill(factor * factor)(rgbArray(x)(y)), 0, factor)
      }
    }
  }

  main(Array.empty)

}


