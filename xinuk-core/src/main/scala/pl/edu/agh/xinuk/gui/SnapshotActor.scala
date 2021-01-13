package pl.edu.agh.xinuk.gui

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridCellId
import pl.edu.agh.xinuk.simulation.WorkerActor.{GridInfo, MsgWrapper, SubscribeGridInfo}

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable

class SnapshotActor private(worker: ActorRef,
                            simulationId: String,
                            workerIds: Set[WorkerId])
                           (implicit config: XinukConfig) extends Actor with ActorLogging {

  override def receive: Receive = started

  private lazy val snapshotSaver: SnapshotSaver = new SnapshotSaver(simulationId)
  val cellColorsStash: mutable.Map[Long, Seq[Seq[(CellId, Color)]]] = mutable.Map.empty.withDefaultValue(Seq.empty)

  override def preStart(): Unit = {
    workerIds.foreach(worker ! MsgWrapper(_, SubscribeGridInfo()))
    log.info("GUI started")
  }

  override def postStop(): Unit = {
    log.info("GUI stopped")
  }

  def started: Receive = {
    case GridInfo(iteration, cellColors, _) =>
      cellColorsStash(iteration) :+= cellColors.toSeq
      if (cellColorsStash(iteration).size == workerIds.size) {
        snapshotSaver.snapshot(iteration, cellColorsStash(iteration).flatten.toMap)
        cellColorsStash.remove(iteration)
      }
  }
}

object SnapshotActor {
  def props(worker: ActorRef, simulationId: String, workerIds: Set[WorkerId])
           (implicit config: XinukConfig): Props = {
    Props(new SnapshotActor(worker, simulationId, workerIds))
  }
}

private class SnapshotSaver(simulationId: String)(implicit config: XinukConfig) {
  private val snapshotDirectory = new File(s"out/snapshots/$simulationId")
  snapshotDirectory.mkdirs()
  private val img = new BufferedImage(config.worldWidth * config.guiCellSize, config.worldHeight * config.guiCellSize, BufferedImage.TYPE_INT_ARGB)


  private def fillImage(cellColors: Map[CellId, Color]): Unit = cellColors.foreach {
    case (GridCellId(x, y), color) =>
      val startX = x * config.guiCellSize
      val startY = y * config.guiCellSize
      img.setRGB(startX, startY, config.guiCellSize, config.guiCellSize, Array.fill(config.guiCellSize * config.guiCellSize)(color.getRGB), 0, config.guiCellSize)
    case _ =>
  }

  def snapshot(iteration: Long, cellColors: Map[CellId, Color]): Unit = {
    val snapshotFile = new File(snapshotDirectory, f"$iteration%09d.png")
    fillImage(cellColors)
    ImageIO.write(img, "png", snapshotFile)
  }
}





