package pl.edu.agh.formin

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.algorithm.ForminMovesController
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.{AlgaeAccessible, ForaminiferaAccessible}
import pl.edu.agh.formin.model.parallel.ForminConflictResolver
import pl.edu.agh.xinuk.model.{EmptyCell, Grid, WorkerId}
import pl.edu.agh.xinuk.model.parallel.{Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.simulation.WorkerActor

import scala.util.{Failure, Random, Success, Try}

object Simulation extends LazyLogging {
  final val ForminConfigPrefix = "formin"

  private val random = new Random(0)

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

  private val grid =  {
    val gridSize = 258
    val grod = Grid.emptyWithDefinedSize(Set.empty, gridSize)
    var foraminiferaCount = 0L
    var algaeCount = 0L
    for {
      x <- 0 until gridSize
      y <- 0 until gridSize
      if x != 0 && y != 0 && x != gridSize - 1 && y != gridSize - 1
    } {
      if (random.nextDouble() < config.spawnChance) {
        grod.cells(x)(y) =
          if (random.nextDouble() < config.foraminiferaSpawnChance) {
            foraminiferaCount += 1
            ForaminiferaAccessible.unapply(EmptyCell.Instance).withForaminifera(config.foraminiferaStartEnergy, 0)
          }
          else {
            algaeCount += 1
            AlgaeAccessible.unapply(EmptyCell.Instance).withAlgae(0)
          }
      }
    }
    logger.info("Initial algae "+ algaeCount)
    logger.info("Initial formin "+ foraminiferaCount)
    grod
  }

  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)

  private val workerProps: Props = WorkerActor.props[ForminConfig]((bufferZone, logger, config) =>
    new ForminMovesController(bufferZone, logger)(config), ForminConflictResolver, grid
  )

  ClusterSharding(system).start(
    typeName = WorkerActor.Name,
    entityProps = workerProps,
    settings = ClusterShardingSettings(system),
    extractShardId = WorkerActor.extractShardId,
    extractEntityId = WorkerActor.extractEntityId
  )

  private val WorkerRegionRef: ActorRef = ClusterSharding(system).shardRegion(WorkerActor.Name)

  def main(args: Array[String]): Unit = {
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

