package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.fortwist.algorithm.FortwistUpdateTag.{Add, Change, Remove}
import pl.edu.agh.fortwist.config.FortwistConfig
import pl.edu.agh.fortwist.model.{Foraminifera, Seabed}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model.{CellContents, CellId, CellState, Direction, Signal}

import scala.math.Ordering

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
          (None, Plan(
            Add(Seabed(Seq(Foraminifera()))),
            Change(Seabed(Seq(foraminifera.changed(-config.foraminiferaReproductionCost))))
          ))
        } else if (currentAlgae > config.algaeEnergeticCapacity) {
          consumedAlgaeCount += config.algaeEnergeticCapacity
          currentAlgae -= config.algaeEnergeticCapacity
          (None, Plan(
            Change(Seabed(Seq(foraminifera.changed(config.algaeEnergeticCapacity)), -config.algaeEnergeticCapacity))
          ))
        } else if (foraminifera.energy < config.foraminiferaLifeActivityCost) {
          foraminiferaDeaths += 1
          foraminiferaTotalLifespan += foraminifera.lifespan
          (None, Plan(
            Remove(Seabed(Seq(foraminifera)))
          ))
        } else {
          foraminiferaMoves += 1
          val direction: Direction = neighbourContents.keys.toSeq
            .map { direction => (direction, cellState.signalMap(direction)) }
            .sortBy(_._2)(Ordering[Signal].reverse)
            .head._1
          (Some(direction), Plan(
            Add(Seabed(Seq(foraminifera.changed(-config.foraminiferaLifeActivityCost)))),
            Remove(Seabed(Seq(foraminifera)))
          ))
        }
    }

    val algaeGrowthPlan = Plan(Add(Seabed(Seq(), Seabed.algaeGrowth(currentAlgae))))

    val outwardsPlans: Map[Direction, Seq[Plan]] = forminPlans.filter(_._1.isDefined)
      .map { case (dirOpt, plan) => (dirOpt.get, plan) }
      .groupBy(_._1)
      .map { case (dir, tuples) => (dir, tuples.map(_._2))}

    val localPlans: Seq[Plan] = forminPlans.filter(_._1.isEmpty)
      .map(_._2)

    (Plans(outwardsPlans, localPlans :+ algaeGrowthPlan), FortwistMetrics(
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
