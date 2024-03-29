package pl.edu.agh.xinuk

import java.awt.Color
import java.io.File
import java.util.UUID
import akka.actor.{ActorRef, ActorSystem}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.xinuk.algorithm.{Metrics, PlanCreator, PlanResolver, WorldCreator}
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.gui.{GridGuiActor, SnapshotActor, SplitSnapshotActor}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.{GridWorldShard, GridWorldType}
import pl.edu.agh.xinuk.simulation.WorkerActor

import scala.util.{Failure, Success, Try}

class Simulation[ConfigType <: XinukConfig : ValueReader](
  configPrefix: String,
  metricHeaders: Vector[String],
  worldCreator: WorldCreator[ConfigType],
  planCreatorFactory: () => PlanCreator[ConfigType],
  planResolverFactory: () => PlanResolver[ConfigType],
  emptyMetrics: => Metrics,
  signalPropagation: SignalPropagation,
  cellToColor: PartialFunction[CellState, Color] = PartialFunction.empty
) extends LazyLogging {

  private val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("xinuk.conf")))
      .filter(_.hasPath(configPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.empty()
      }.withFallback(ConfigFactory.load("cluster.conf"))

  implicit val config: ConfigType = {
    val applicationConfig = rawConfig.getConfig(configPrefix)
    logger.info(WorkerActor.MetricsMarker, applicationConfig.root().render(ConfigRenderOptions.concise()))
    logger.info(WorkerActor.MetricsMarker, logHeader)

    import net.ceedubs.ficus.Ficus._
    Try(applicationConfig.as[ConfigType]("config")) match {
      case Success(parsedConfig) =>
        logger.info("Config parsed successfully.")
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }
  }

  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)

  private val workerRegionRef: ActorRef = ClusterSharding(system).start(
    typeName = WorkerActor.Name,
    entityProps = WorkerActor.props[ConfigType](workerRegionRef, planCreatorFactory(), planResolverFactory(), emptyMetrics, signalPropagation, cellToColor),
    settings = ClusterShardingSettings(system),
    extractShardId = WorkerActor.extractShardId,
    extractEntityId = WorkerActor.extractEntityId
  )

  def start(): Unit = {
    if (config.isSupervisor) {
      val workerToWorld: Map[WorkerId, WorldShard] = worldCreator.prepareWorld().build()
      val simulationId: String = UUID.randomUUID().toString

      workerToWorld.foreach( { case (workerId, world) =>
        WorkerActor.send(workerRegionRef, workerId, WorkerActor.WorkerInitialized(world))
      })

      (config.guiType, config.worldType) match {
        case (GuiType.None, _) =>
        case (GuiType.Grid, GridWorldType) =>
          workerToWorld.foreach( { case (workerId, world) =>
            system.actorOf(GridGuiActor.props(workerRegionRef, simulationId, workerId, world.asInstanceOf[GridWorldShard].bounds))
          })
        case (GuiType.SplitSnapshot, GridWorldType) =>
          workerToWorld.foreach( { case (workerId, world) =>
            system.actorOf(SplitSnapshotActor.props(workerRegionRef, simulationId, workerId, world.asInstanceOf[GridWorldShard].bounds))
          })
        case (GuiType.Snapshot, GridWorldType) =>
          system.actorOf(SnapshotActor.props(workerRegionRef, simulationId, workerToWorld.keySet))
        case _ => logger.warn("GUI type not recognized or incompatible with World format.")
      }
    }
  }

  private def logHeader: String = s"worker:iteration;activeTime;waitingTime;${metricHeaders.mkString(";")}"
}