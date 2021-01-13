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

class SplitSnapshotActor private(worker: ActorRef,
                                 simulationId: String,
                                 workerId: WorkerId,
                                 bounds: GridWorldShard.Bounds)
                                (implicit config: XinukConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val snapshotSaver: SplitSnapshotSaver = new SplitSnapshotSaver(bounds, simulationId, workerId)

  override def preStart(): Unit = {
    worker ! MsgWrapper(workerId, SubscribeGridInfo())
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
  }

  def started: Receive = {
    case GridInfo(iteration, cellColors, _) =>
      snapshotSaver.snapshot(iteration, cellColors)
  }
}

object SplitSnapshotActor {
  def props(worker: ActorRef, simulationId: String, workerId: WorkerId, bounds: GridWorldShard.Bounds)
           (implicit config: XinukConfig): Props = {
    Props(new SplitSnapshotActor(worker, simulationId, workerId, bounds))
  }
}

private class SplitSnapshotSaver(bounds: GridWorldShard.Bounds, simulationId: String, workerId: WorkerId)
                           (implicit config: XinukConfig) {
  private val snapshotDirectory = new File(s"out/snapshots/$simulationId")
  snapshotDirectory.mkdirs()
  private val img = new BufferedImage(bounds.xSize * config.guiCellSize, bounds.ySize * config.guiCellSize, BufferedImage.TYPE_INT_ARGB)


  private def fillImage(cellColors: Map[CellId, Color]): Unit = cellColors.foreach {
    case (GridCellId(x, y), color) =>
      val startX = (x - bounds.xMin) * config.guiCellSize
      val startY = (y - bounds.yMin) * config.guiCellSize
      img.setRGB(startX, startY, config.guiCellSize, config.guiCellSize, Array.fill(config.guiCellSize * config.guiCellSize)(color.getRGB), 0, config.guiCellSize)
    case _ =>
  }

  def snapshot(iteration: Long, cellColors: Map[CellId, Color]): Unit = {
    val snapshotFile = new File(snapshotDirectory, f"${workerId.value}%04d_$iteration%09d.png")
    fillImage(cellColors)
    ImageIO.write(img, "png", snapshotFile)
  }
}





