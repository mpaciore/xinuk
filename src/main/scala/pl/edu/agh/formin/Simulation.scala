package pl.edu.agh.formin

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.config.ForminConfig
import pl.edu.agh.formin.model.parallel.{Neighbour, NeighbourPosition}

import scala.collection.immutable.TreeMap
import scala.util.{Failure, Success, Try}

object Simulation extends App with LazyLogging {
  final val ForminConfigPrefix = "formin"

  private val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("formin.conf")))
      .filter(_.hasPath(ForminConfigPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.load()
      }

  implicit val config: ForminConfig =
    ForminConfig.fromConfig(rawConfig.getConfig(ForminConfigPrefix)) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }

  private val system = ActorSystem(rawConfig.getString("application.name"))
  private val workers: TreeMap[WorkerId, ActorRef] =
    (1 to math.pow(config.workersRoot, 2).toInt).map { i =>
      val workerId = WorkerId(i)
      workerId -> system.actorOf(WorkerActor.props(workerId))
    }(collection.breakOut)


  workers.foreach { case (id, ref) =>
    val neighbours = NeighbourPosition.values.flatMap { pos =>
      pos.neighbourId(id).map(id => Neighbour(pos, workers(id)))
    }
    ref ! WorkerActor.NeighboursInitialized(neighbours.toSet)
  }

}

