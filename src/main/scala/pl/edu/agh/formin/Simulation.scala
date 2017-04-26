package pl.edu.agh.formin

import java.io.File

import akka.actor.{ActorSystem, Props}
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
  private val workerId = WorkerId(1)
  private val worker = system.actorOf(WorkerActor.props(workerId))
  private val scheduler = system.actorOf(Props(classOf[SchedulerActor], Vector(worker)))

  config.guiType match {
    case tpe: GuiType.Basic.type => system.actorOf(GuiActor.props(TreeMap(workerId -> worker), Left(tpe)))
    case tpe: GuiType.Signal.type => system.actorOf(GuiActor.props(TreeMap(workerId -> worker), Right(tpe)))
    case _ =>
  }

  scheduler ! SchedulerActor.StartSimulation(config.iterationsNumber)

}

