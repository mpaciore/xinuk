package pl.edu.agh.formin

import java.io.File

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.formin.config.ForminConfig

import scala.util.{Failure, Success, Try}

object Simulation extends App with LazyLogging {
  final val ForminConfigPrefix = "formin"
  private val rawConfig =
    Try(ConfigFactory.parseFile(new File("formin.conf")).getConfig(ForminConfigPrefix)).getOrElse {
      logger.info("Falling back to reference.conf")
      ConfigFactory.load().getConfig(ForminConfigPrefix)
    }

  implicit val config =
    ForminConfig.fromConfig(rawConfig) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
    }

}
