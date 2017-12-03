package pl.edu.agh.formin

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.{Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.model.WorkerId

import scala.util.{Failure, Success, Try}

object Simulation extends LazyLogging {
  final val ForminConfigPrefix = "formin"

  private val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("formin.conf")))
      .filter(_.hasPath(ForminConfigPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.empty()
      }.withFallback(ConfigFactory.load("cluster.conf"))
      .withFallback(ConfigFactory.load())

  implicit val config: ForminConfig = {
    val forminConfig = rawConfig.getConfig(ForminConfigPrefix)
    logger.info(WorkerActor.MetricsMarker, forminConfig.root().render(ConfigRenderOptions.concise()))
    logger.info(WorkerActor.MetricsMarker, "worker:foraminiferaCount;algaeCount;foraminiferaDeaths;foraminiferaTotalEnergy;foraminiferaReproductionsCount;consumedAlgaeCount;foraminiferaTotalLifespan;algaeTotalLifespan")
    ForminConfig.fromConfig(forminConfig) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }
  }

  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)

  ClusterSharding(system).start(
    typeName = WorkerActor.Name,
    entityProps = WorkerActor.props,
    settings = ClusterShardingSettings(system),
    extractShardId = WorkerActor.extractShardId,
    extractEntityId = WorkerActor.extractEntityId
  )

  val WorkerRegionRef: ActorRef = ClusterSharding(system).shardRegion(WorkerActor.Name)

  def main(args: Array[String]): Unit = {
    if (config.isSupervisor) {

      val workers: Vector[WorkerId] =
        (1 to math.pow(config.workersRoot, 2).toInt)
          .map(WorkerId)(collection.breakOut)

      workers.foreach { id =>
        val neighbours: Vector[Neighbour] = NeighbourPosition.values.flatMap { pos =>
          pos.neighbourId(id).map(_ => Neighbour(pos))
        }(collection.breakOut)
        WorkerRegionRef ! WorkerActor.NeighboursInitialized(id, neighbours)
      }
    }
  }

}

