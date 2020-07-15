package pl.edu.agh.xinuk

import java.awt.Color
import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.xinuk.algorithm.{PlanCreator, PlanResolver, WorldCreator}
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.gui.GuiActor
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.grid.GridWorld
import pl.edu.agh.xinuk.simulation.{Metrics, WorkerActor}

import scala.util.{Failure, Success, Try}

class Simulation[ConfigType <: XinukConfig : ValueReader](
  configPrefix: String,
  metricHeaders: Vector[String],
  gridCreator: WorldCreator[ConfigType],
  planCreatorFactory: () => PlanCreator[ConfigType],
  planResolverFactory: () => PlanResolver[ConfigType],
  emptyMetrics: => Metrics,
  signalPropagation: SignalPropagation,
  cellToColor: PartialFunction[CellState, Color] = PartialFunction.empty
)(implicit directions: Seq[Direction]) extends LazyLogging {

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
      entityProps = WorkerActor.props[ConfigType](workerRegionRef, planCreatorFactory(), planResolverFactory(), emptyMetrics, signalPropagation),
      settings = ClusterShardingSettings(system),
      extractShardId = WorkerActor.extractShardId,
      extractEntityId = WorkerActor.extractEntityId
    )

  def start(): Unit = {
    if (config.isSupervisor) {
      val workerToWorld: Map[WorkerId, World] = gridCreator.prepareWorld().build()

      workerToWorld.foreach( { case (workerId, world) =>
        (config.guiType, world) match {
          case (GuiType.None, _) =>
          case (GuiType.Grid, gridWorld: GridWorld) =>
            system.actorOf(GuiActor.props(workerRegionRef, workerId, gridWorld.span, cellToColor))
          case _ => logger.warn("GUI type incompatible with World format.")
        }
        workerRegionRef ! WorkerActor.WorkerInitialized(workerId, world)
      })
    }
  }

  private def logHeader: String = s"worker:${metricHeaders.mkString(";")}"
}