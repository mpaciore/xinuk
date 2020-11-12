package pl.edu.agh.xinuk.gui

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import javax.imageio.ImageIO
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldShard}
import pl.edu.agh.xinuk.simulation.WorkerActor.{GridInfo, MsgWrapper, SubscribeGridInfo}

import scala.util.Random

class SnapshotActor private(worker: ActorRef,
                            simulationId: String,
                            workerId: WorkerId,
                            bounds: GridWorldShard.Bounds,
                            cellToColor: PartialFunction[CellState, Color])
                           (implicit config: XinukConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val img: SnapshotSaver = new SnapshotSaver(bounds, simulationId, workerId, cellToColor)

  override def preStart(): Unit = {
    worker ! MsgWrapper(workerId, SubscribeGridInfo())
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
  }

  def started: Receive = {
    case GridInfo(iteration, cells, _) =>
      img.snapshot(iteration, cells)
  }
}

object SnapshotActor {
  def props(worker: ActorRef, simulationId: String, workerId: WorkerId, bounds: GridWorldShard.Bounds, cellToColor: PartialFunction[CellState, Color])
           (implicit config: XinukConfig): Props = {
    Props(new SnapshotActor(worker, simulationId, workerId, bounds, cellToColor))
  }
}

private class SnapshotSaver(bounds: GridWorldShard.Bounds, simulationId: String, workerId: WorkerId, cellToColor: PartialFunction[CellState, Color])
                           (implicit config: XinukConfig) {
  private val snapshotDirectory = new File(s"out/snapshots/$simulationId")
  snapshotDirectory.mkdirs()

  private val obstacleColor = new swing.Color(0, 0, 0)
  private val emptyColor = new swing.Color(255, 255, 255)
  private val img = new BufferedImage(bounds.xSize * config.guiCellSize, bounds.ySize * config.guiCellSize, BufferedImage.TYPE_INT_ARGB)

  private def defaultColor: CellState => Color = state =>
    state.contents match {
      case Obstacle => obstacleColor
      case Empty => emptyColor
      case other =>
        val random = new Random(other.getClass.hashCode())
        val hue = random.nextFloat()
        val saturation = 1.0f
        val luminance = 0.6f
        Color.getHSBColor(hue, saturation, luminance)
    }

  private def fillImage(cells: Set[Cell]): Unit = cells.foreach {
    case Cell(GridCellId(x, y), state) =>
      val startX = (x - bounds.xMin) * config.guiCellSize
      val startY = (y - bounds.yMin) * config.guiCellSize
      val color: Color = cellToColor.applyOrElse(state, defaultColor)
      img.setRGB(startX, startY, config.guiCellSize, config.guiCellSize, Array.fill(config.guiCellSize * config.guiCellSize)(color.getRGB), 0, config.guiCellSize)
    case _ =>
  }

  def snapshot(iteration: Long, cells: Set[Cell]): Unit = {
    val snapshotFile = new File(snapshotDirectory, f"${workerId.value}%04d_$iteration%05d.png")
    fillImage(cells)
    ImageIO.write(img, "png", snapshotFile)
  }
}





