package pl.edu.agh.xinuk

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import org.slf4j.Logger
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.WorkerId
import pl.edu.agh.xinuk.model.parallel.{ConflictResolver, Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.simulation.WorkerActor

import scala.collection.immutable.TreeSet
import scala.util.{Failure, Success, Try}

class Simulation[ConfigType <: XinukConfig](
  configPrefix: String,
  metricHeaders: Vector[String],
  conflictResolver: ConflictResolver[ConfigType])(
  movesControllerFactory: (TreeSet[(Int, Int)], Logger, ConfigType) => MovesController)(
  //todo remove
  configFactory: Config => Try[ConfigType]) extends LazyLogging {

  private val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("xinuk.conf")))
      .filter(_.hasPath(configPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.empty()
      }.withFallback(ConfigFactory.load("cluster.conf"))
      .withFallback(ConfigFactory.load())

  private def logHeader: String = s"worker:${metricHeaders.mkString(";")}"

  implicit val config: ConfigType = {
    val forminConfig = rawConfig.getConfig(configPrefix)
    logger.info(WorkerActor.MetricsMarker, forminConfig.root().render(ConfigRenderOptions.concise()))
    logger.info(WorkerActor.MetricsMarker, logHeader)
    configFactory(forminConfig) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }
  }

  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)

  private val workerProps: Props = WorkerActor.props[ConfigType](movesControllerFactory, conflictResolver)

  ClusterSharding(system).start(
    typeName = WorkerActor.Name,
    entityProps = workerProps,
    settings = ClusterShardingSettings(system),
    extractShardId = WorkerActor.extractShardId,
    extractEntityId = WorkerActor.extractEntityId
  )

  private val WorkerRegionRef: ActorRef = ClusterSharding(system).shardRegion(WorkerActor.Name)

  def start(): Unit = {
    if (config.isSupervisor) {

      val workers: Vector[WorkerId] =
        (1 to math.pow(config.workersRoot, 2).toInt)
          .map(WorkerId)(collection.breakOut)

      workers.foreach { id =>
        val neighbours: Vector[Neighbour] = NeighbourPosition.values.flatMap { pos =>
          pos.neighbourId(id).map(_ => Neighbour(pos))
        }(collection.breakOut)
        WorkerRegionRef ! WorkerActor.NeighboursInitialized(id, neighbours, WorkerRegionRef)
      }
    }
  }

}