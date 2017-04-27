package pl.edu.agh.formin

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.gui.GuiActor

import scala.collection.immutable.TreeMap
import scala.util.{Failure, Success, Try}

object Simulation extends App with LazyLogging {
  final val ForminConfigPrefix = "formin"
  private val rawConfig =
    Try(ConfigFactory.parseFile(new File("formin.conf")).getConfig(ForminConfigPrefix)).getOrElse {
      logger.info("Falling back to reference.conf")
      ConfigFactory.load().getConfig(ForminConfigPrefix)
    }

  implicit val config: ForminConfig =
    ForminConfig.fromConfig(rawConfig) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }

  private val system = ActorSystem("formin")
  private val workers: TreeMap[WorkerId, ActorRef] =
    (1 to math.pow(config.workersRoot, 2).toInt).map { i =>
      val workerId = WorkerId(i)
      workerId -> system.actorOf(WorkerActor.props(workerId))
    }(collection.breakOut)

  private val scheduler = system.actorOf(Props(classOf[SchedulerActor], workers.values.toVector))

  config.guiType match {
    case tpe: GuiType.Basic.type => system.actorOf(GuiActor.props(workers, Left(tpe)))
    case tpe: GuiType.Signal.type => system.actorOf(GuiActor.props(workers, Right(tpe)))
    case _ =>
  }

  scheduler ! SchedulerActor.StartSimulation(config.iterationsNumber)

}

