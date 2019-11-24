package pl.edu.agh.xinuk

import java.awt.Color
import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.LazyLogging
import net.ceedubs.ficus.readers.ValueReader
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.gui.GuiActor
import pl.edu.agh.xinuk.model.Grid.CellArray
import pl.edu.agh.xinuk.model.parallel.{ConflictResolver, Neighbour, NeighbourPosition}
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.simulation.WorkerActor

import scala.collection.immutable.TreeSet
import scala.util.{Failure, Success, Try}

class Simulation[ConfigType <: XinukConfig : ValueReader](
  configPrefix: String,
  metricHeaders: Vector[String],
  conflictResolver: ConflictResolver[ConfigType],
  smellPropagationFunction: XinukConfig => (CellArray, Int, Int) => Vector[Option[Signal]],
  emptyCellFactory: => SmellingCell = EmptyCell.Instance)(
  movesControllerFactory: (TreeSet[(Int, Int)], ConfigType) => MovesController,
  cellToColor: PartialFunction[GridPart, Color] = PartialFunction.empty
) extends LazyLogging {

  private val rawConfig: Config =
    Try(ConfigFactory.parseFile(new File("xinuk.conf")))
      .filter(_.hasPath(configPrefix))
      .getOrElse {
        logger.info("Falling back to reference.conf")
        ConfigFactory.empty()
      }.withFallback(ConfigFactory.load("cluster.conf"))

  private def logHeader: String = s"worker:${metricHeaders.mkString(";")}"

  implicit val config: ConfigType = {
    val conf = rawConfig.getConfig(configPrefix)
    logger.info(WorkerActor.MetricsMarker, conf.root().render(ConfigRenderOptions.concise()))
    logger.info(WorkerActor.MetricsMarker, logHeader)

    import net.ceedubs.ficus.Ficus._
    Try(conf.as[ConfigType]("config")) match {
      case Success(parsedConfig) =>
        parsedConfig
      case Failure(parsingError) =>
        logger.error("Config parsing error.", parsingError)
        System.exit(2)
        throw new IllegalArgumentException
    }
  }

  private val system = ActorSystem(rawConfig.getString("application.name"), rawConfig)
  private val workerRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = WorkerActor.Name,
      entityProps = WorkerActor.props[ConfigType](
        workerRegionRef,
        movesControllerFactory,
        conflictResolver,
        smellPropagationFunction(config.asInstanceOf[XinukConfig]),
        emptyCellFactory
      ),
      settings = ClusterShardingSettings(system),
      extractShardId = WorkerActor.extractShardId,
      extractEntityId = WorkerActor.extractEntityId
    )

  def start(): Unit = {
    if (config.isSupervisor) {

      val workers: Vector[WorkerId] =
        (1 to math.pow(config.workersRoot, 2).toInt)
          .map(WorkerId)(collection.breakOut)

      workers.foreach { id =>
        if (config.guiType != GuiType.None) {
          system.actorOf(GuiActor.props(workerRegionRef, id, cellToColor))
        }
        val neighbours: Vector[Neighbour] = NeighbourPosition.values.flatMap { pos =>
          pos.neighbourId(id).map(_ => Neighbour(pos))
        }(collection.breakOut)
        workerRegionRef ! WorkerActor.NeighboursInitialized(id, neighbours)
      }
    }
  }

}