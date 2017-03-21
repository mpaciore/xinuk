package pl.edu.agh.formin

import akka.actor.{Actor, ActorLogging, ActorRef}
import pl.edu.agh.formin.SchedulerActor.Register
import pl.edu.agh.formin.IterationStatus
import pl.edu.agh.formin.Gui._
import pl.edu.agh.formin.model._
import java.awt.image.BufferedImage
import java.awt.{Color, Dimension, Font}
import javax.swing.{ImageIcon, UIManager}

import scala.language.postfixOps
import scala.swing.BorderPanel.Position._
import scala.swing.TabbedPane.Page
import scala.swing._

class Gui(scheduler: ActorRef, worker: WorkerId) extends Actor with ActorLogging{

  override def receive: Receive = started

  private var gui: GuiGrid = _

  override def preStart = {
    scheduler ! Register
    log.info("GUI started")
  }

  def started: Receive = {
    case NewIteration(state) =>
      val grid : Grid = state.getGridForWorker(worker)
      if(gui==null) gui = new GuiGrid(grid)
      else gui.setNewValues(grid)
  }
}

object Gui {

  case class NewIteration(state: IterationStatus) extends AnyVal

  case object CloseGui

  sealed trait State

  object State {

    case object Started extends State

  }

}

case class GuiGrid (initialState : Grid) extends SimpleSwingApplication {

  {
    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    } catch {
      case t: Throwable =>
    }
  }
  val bgcolor = new Color(220, 220, 220)
  val textFont = new Font("Arial", Font.BOLD, 14)
  val dim = initialState.cells.length
  val pc = new ParticleCanvas(dim, initialState.cells)
  val chartPanel = new BorderPanel {
    background = bgcolor
  }

  def top = new MainFrame {
    title = "Ising model"
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

  def setNewValues(newGrid : Grid){
    val dim = newGrid.cells.length
    for (x <- 0 until dim;
         y <- 0 until dim)
      pc.set(x, y, newGrid.cells(x)(y))
  }

  class ParticleCanvas private(dim: Int) extends Label {
    val algaeColor = new swing.Color(9, 108, 16).getRGB
    val forminColor = new swing.Color(81, 71, 8).getRGB
    val obstacleColor = new swing.Color(0, 0, 0).getRGB
    val emptyColor = new swing.Color(255, 255, 255).getRGB
    val img = new BufferedImage(dim, dim, BufferedImage.TYPE_INT_ARGB)

    def this(dim: Int, init: Array[Array[Cell]]) {
      this(dim)
      for (x <- 0 until dim;
           y <- 0 until dim)
        set(x, y, init(x)(y))
    }

    icon = new ImageIcon(img)

    def set(x: Int, y: Int, cell: Cell): Unit = {
      if (cell.isInstanceOf[AlgaeCell])
        img.setRGB(x, y, algaeColor)
      if (cell.isInstanceOf[ForaminiferaCell])
        img.setRGB(x, y, forminColor)
      //if (cell.isInstanceOf[Obstacle])
       // img.setRGB(x, y, obstacleColor)
      else
        img.setRGB(x, y, emptyColor)
    }
  }

}


