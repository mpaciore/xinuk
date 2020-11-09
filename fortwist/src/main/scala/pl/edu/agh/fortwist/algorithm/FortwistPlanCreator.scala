package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.fortwist.algorithm.FortwistUpdate.SeabedUpdate
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.{Foraminifera, Seabed}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model.{CellContents, CellId, CellState, Direction}

final case class FortwistPlanCreator() extends PlanCreator[FortwistConfig] {
  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: FortwistConfig): (Plans, FortwistMetrics) = {
    val seabed = cellState.contents.asInstanceOf[Seabed]

    val foraminiferaCount = seabed.foraminiferas.size
    val algaeCount = seabed.algae
    val foraminiferaTotalEnergy = seabed.foraminiferas.map(_.energy).sum

    var foraminiferaDeaths = 0L
    var foraminiferaReproductionsCount = 0L
    var consumedAlgaeCount = 0.0
    var foraminiferaTotalLifespan = 0L
    var foraminiferaMoves = 0L

    var currentAlgae = seabed.algae

    val forminPlans: Seq[(Option[Direction], Plan)] = seabed.foraminiferas.map {
      foraminifera =>
        if (foraminifera.energy > config.foraminiferaReproductionThreshold) {
          foraminiferaReproductionsCount += 1
          None -> Plan(
            SeabedUpdate(foraminiferasToAdd = Seq(Foraminifera())),
            SeabedUpdate(foraminiferasToChange = Seq(foraminifera.changed(-config.foraminiferaReproductionCost)))
          )
        } else if (currentAlgae > config.algaeEnergeticCapacity) {
          consumedAlgaeCount += config.algaeEnergeticCapacity
          currentAlgae -= config.algaeEnergeticCapacity
          None -> Plan(
            SeabedUpdate(foraminiferasToChange = Seq(foraminifera.changed(config.algaeEnergeticCapacity)), algaeDiff = -config.algaeEnergeticCapacity)
          )
        } else if (foraminifera.energy < config.foraminiferaLifeActivityCost) {
          foraminiferaDeaths += 1
          foraminiferaTotalLifespan += foraminifera.lifespan
          None -> Plan(
            SeabedUpdate(foraminiferasToRemove = Seq(foraminifera.id))
          )
        } else {
          foraminiferaMoves += 1
          val direction: Direction = neighbourContents.keys.toSeq
            .map { direction => (direction, cellState.signalMap(direction)) }
            .maxBy(_._2)._1
          Some(direction) -> Plan(
            SeabedUpdate(foraminiferasToAdd = Seq(foraminifera.changed(-config.foraminiferaLifeActivityCost))),
            SeabedUpdate(foraminiferasToRemove = Seq(foraminifera.id))
          )
        }
    }

    val algaeGrowthPlan = Plan(SeabedUpdate(algaeDiff = Seabed.algaeGrowth(currentAlgae)))

    (Plans(forminPlans :+ (None -> algaeGrowthPlan)), FortwistMetrics(
      foraminiferaCount,
      algaeCount,
      foraminiferaDeaths,
      foraminiferaTotalEnergy,
      foraminiferaReproductionsCount,
      consumedAlgaeCount,
      foraminiferaTotalLifespan,
      foraminiferaMoves
    ))
  }

}
