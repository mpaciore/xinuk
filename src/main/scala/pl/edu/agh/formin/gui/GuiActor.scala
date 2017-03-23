package pl.edu.agh.formin.gui

import java.awt.image.BufferedImage
import java.awt.{Color, Dimension}
import javax.swing.{ImageIcon, UIManager}

import akka.actor.{Actor, ActorLogging, ActorRef}
import pl.edu.agh.formin.SchedulerActor.Register
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model._
import pl.edu.agh.formin.{IterationStatus, WorkerId}

import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.util.Try

class GuiActor(scheduler: ActorRef, worker: WorkerId)(implicit config: ForminConfig) extends Actor with ActorLogging {

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
}

private[gui] class GuiGrid(dimension: Int) extends SimpleSwingApplication {

  Try(UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName))

  private val bgcolor = new Color(220, 220, 220)
  private val pc = new ParticleCanvas(dimension)

  def top = new MainFrame {
    title = "Formin model"
    minimumSize = new Dimension(1050, 700)
    background = bgcolor

    val canvas = new BorderPanel {
      background = bgcolor
      layout(pc) = Center
    }
    val simPanel = new BorderPanel {
      background = bgcolor
      layout(canvas) = Center
    }
    contents = new TabbedPane {
      pages += new Page("Simulation", simPanel)
    }
  }

  def setNewValues(newGrid: Grid): Unit = {
    pc.set(newGrid.cells)
    pc.repaint()
  }

  private class ParticleCanvas(dimension: Int) extends Label {
    private val algaeColor = new swing.Color(9, 108, 16).getRGB
    private val forminColor = new swing.Color(81, 71, 8).getRGB
    private val obstacleColor = new swing.Color(0, 0, 0).getRGB
    private val emptyColor = new swing.Color(255, 255, 255).getRGB
    private val img = new BufferedImage(dimension, dimension, BufferedImage.TYPE_INT_ARGB)

    icon = new ImageIcon(img)

    def set(cells: Array[Array[Cell]]): Unit = {
      val rgbArray = cells.flatMap(_.map {
        case AlgaeCell(_) => algaeColor
        case ForaminiferaCell(_, _) => forminColor
        case Obstacle => obstacleColor
        case EmptyCell(_) => emptyColor
      })
      img.setRGB(0, 0, dimension, dimension, rgbArray, 0, dimension)
    }
  }

}


