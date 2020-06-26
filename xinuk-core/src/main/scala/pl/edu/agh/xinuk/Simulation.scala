package pl.edu.agh.xinuk

import java.awt.Color
import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.xinuk.algorithm.{GridCreator, PlanCreator, PlanResolver}
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.gui.GuiActor
import pl.edu.agh.xinuk.model.Cell.SmellMap
import pl.edu.agh.xinuk.model.EnhancedCell.NeighbourMap
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.{Metrics, WorkerActor}

import scala.util.{Failure, Success, Try}

class Simulation[ConfigType <: XinukConfig : ValueReader](
  configPrefix: String,
  metricHeaders: Vector[String],
  gridCreator: GridCreator[ConfigType],
  planCreator: PlanCreator[ConfigType],
  planResolver: PlanResolver[ConfigType],
  emptyMetricsFactory: () => Metrics,
  smellPropagationFunction: (EnhancedGrid, NeighbourMap) => SmellMap,
  cellToColor: PartialFunction[Cell, Color] = PartialFunction.empty
) extends LazyLogging {

  private val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("xinuk.conf")))
      .filter(_.hasPath(configPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.empty()
      }.withFallback(ConfigFactory.load("cluster.conf"))
  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)

  implicit val config: ConfigType = {
    val forminConfig = rawConfig.getConfig(configPrefix)
    logger.info(WorkerActor.MetricsMarker, forminConfig.root().render(ConfigRenderOptions.concise()))
    logger.info(WorkerActor.MetricsMarker, logHeader)

    import net.ceedubs.ficus.Ficus._
    Try(forminConfig.as[ConfigType]("config")) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }
  }
  private val workerRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = WorkerActor.Name,
      entityProps = WorkerActor.props[ConfigType](workerRegionRef, planCreator, planResolver, emptyMetricsFactory, smellPropagationFunction),
      settings = ClusterShardingSettings(system),
      extractShardId = WorkerActor.extractShardId,
      extractEntityId = WorkerActor.extractEntityId
    )

  def start(): Unit = {
    if (config.isSupervisor) {
      val workerIds: Vector[WorkerId] = (1 to math.pow(config.workersRoot, 2).toInt).map(WorkerId)(collection.breakOut)

      val (initialGrid, nonPlanarConnections): (Grid, NonPlanarConnections) = gridCreator.initialGrid

      // TODO: simplify to skip this step?
      val enhancedGrid: EnhancedGrid = EnhancedGrid(initialGrid, nonPlanarConnections)

      val dividedGrid: Seq[(WorkerId, EnhancedGrid, Set[WorkerId])] =
        enhancedGrid.divide(config.workersRoot, workerIds)

      val workerToIncomingNeighbours: Map[WorkerId, Set[WorkerId]] = workerIds.map({ id =>
        (id, dividedGrid.filter(_._3.contains(id)).map(_._1).toSet)
      }).toMap

      val workersInfo: Seq[(WorkerId, EnhancedGrid, Set[WorkerId], Set[WorkerId])] =
        dividedGrid.map({ case (workerId, grid, outgoingNeighbours) =>
            (workerId, grid, outgoingNeighbours, workerToIncomingNeighbours(workerId))})

      workersInfo.foreach { case (workerId, grid, outgoingNeighbours, incomingNeighbours) =>
        if (config.guiType != GuiType.None) {
          system.actorOf(GuiActor.props(workerRegionRef, workerId, grid.xSize, grid.ySize, cellToColor))
        }
        workerRegionRef ! WorkerActor.NeighboursInitialized(workerId, grid, outgoingNeighbours, incomingNeighbours)
      }
    }
  }

  private def logHeader: String = s"worker:${metricHeaders.mkString(";")}"
}