import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

case class Metrics(iteration: Long,
                   foraminiferaCount: Long,
                   algaeCount: Long,
                   foraminiferaDeaths: Long,
                   foraminiferaTotalEnergy: Double,
                   foraminiferaReproductionsCount: Long,
                   consumedAlgaeCount: Long,
                   foraminiferaTotalLifespan: Long,
                   algaeTotalLifespan: Long) {
  def +(other: Metrics) =
    Metrics(
      iteration,
      foraminiferaCount + other.foraminiferaCount,
      algaeCount + other.algaeCount,
      foraminiferaDeaths + other.foraminiferaDeaths,
      foraminiferaTotalEnergy + other.foraminiferaTotalEnergy,
      foraminiferaReproductionsCount + other.foraminiferaReproductionsCount,
      consumedAlgaeCount + other.consumedAlgaeCount,
      foraminiferaTotalLifespan + other.foraminiferaTotalLifespan,
      algaeTotalLifespan + other.algaeTotalLifespan
    )
}


case class ParsedMetrics(iteration: Long,
                         foraminiferaCount: Long,
                         algaeCount: Long,
                         foraminiferaAverageDeadLifespan: Double,
                         algaeAverageDeadLifespan: Double,
                         foraminiferaTotalEnergy: Double,
                         foraminiferaReproductionCount: Long
                        ) {
  override def toString: String = {
    s"$iteration,$foraminiferaCount,$algaeCount,$foraminiferaTotalEnergy,$foraminiferaReproductionCount,$foraminiferaAverageDeadLifespan,$algaeAverageDeadLifespan"
  }
}

/*
worker:foraminiferaCount;algaeCount;foraminiferaDeaths;foraminiferaTotalEnergy;foraminiferaReproductionsCount;consumedAlgaeCount;foraminiferaTotalLifespan;algaeTotalLifespan
 */
object Merge extends App {
  val iterators = args.map { name =>
    Source.fromFile(name)
      .getLines()
      .drop(2) //skip legend
      .map(line =>
      Try(
        line.split(';') match {
          case Array(iteration, fCount, aCount, fDeaths, fTotalEnergy, fReproductions, fConsumed, fTotalLifespan, aTotalLifespan) =>
            Metrics(iteration.toLong, fCount.toLong, aCount.toLong, fDeaths.toLong, fTotalEnergy.toDouble, fReproductions.toLong, fConsumed.toLong, fTotalLifespan.toLong, aTotalLifespan.toLong)
        }
      ).getOrElse(throw new IllegalArgumentException(line))
    ).buffered
  }
  println("iteration,foraminiferaCount,algaeCount,foraminiferaTotalEnergy,foraminiferaReproductionCount,foraminiferaAverageDeadLifespan,algaeAverageDeadLifespan")
  var iteration = 2L
  while (iterators.exists(_.nonEmpty)) {
    val total = iterators.flatMap { iterator =>
      val currentIteration = ListBuffer.empty[Metrics]
      while (iterator.nonEmpty && iterator.head.iteration == iteration) currentIteration += iterator.next()
      currentIteration
    }.reduce(_ + _)
    val result = ParsedMetrics(
      total.iteration,
      total.foraminiferaCount,
      total.algaeCount,
      if (total.foraminiferaDeaths != 0) total.foraminiferaTotalLifespan.toDouble / total.foraminiferaDeaths else 0,
      if (total.consumedAlgaeCount != 0) total.algaeTotalLifespan.toDouble / total.consumedAlgaeCount else 0,
      total.foraminiferaTotalEnergy,
      total.foraminiferaReproductionsCount
    )
    println(result)
    iteration += 1
  }
}

