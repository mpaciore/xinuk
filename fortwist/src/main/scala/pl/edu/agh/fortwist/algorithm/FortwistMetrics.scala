package pl.edu.agh.fortwist.algorithm

import pl.edu.agh.xinuk.algorithm.Metrics

final case class FortwistMetrics(foraminiferaCount: Long,
                                 algaeCount: Double,
                                 foraminiferaDeaths: Long,
                                 foraminiferaTotalEnergy: Double,
                                 foraminiferaReproductionsCount: Long,
                                 consumedAlgaeCount: Double,
                                 foraminiferaTotalLifespan: Long,
                                 foraminiferaMoves: Long) extends Metrics {

  override def log: String = {
    s"$foraminiferaCount;$algaeCount;$foraminiferaDeaths;$foraminiferaTotalEnergy;$foraminiferaReproductionsCount;$consumedAlgaeCount;$foraminiferaTotalLifespan;$foraminiferaMoves"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Foraminifera" -> foraminiferaCount.toDouble,
    "Algae" -> algaeCount.toDouble
  )

  override def +(other: Metrics): FortwistMetrics = {
    other match {
      case FortwistMetrics.Empty => this
      case FortwistMetrics(otherForaminiferaCount, otherAlgaeCount, otherForaminiferaDeaths,
      otherForaminiferaTotalEnergy, otherForaminiferaReproductionsCount, otherConsumedAlgaeCount,
      otherForaminiferaTotalLifespan, otherForaminiferaMoves) =>
        FortwistMetrics(
          foraminiferaCount + otherForaminiferaCount,
          algaeCount + otherAlgaeCount,
          foraminiferaDeaths + otherForaminiferaDeaths,
          foraminiferaTotalEnergy + otherForaminiferaTotalEnergy,
          foraminiferaReproductionsCount + otherForaminiferaReproductionsCount,
          consumedAlgaeCount + otherConsumedAlgaeCount,
          foraminiferaTotalLifespan + otherForaminiferaTotalLifespan,
          foraminiferaMoves + otherForaminiferaMoves)
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-FortwistMetrics to FortwistMetrics")
    }
  }
}

object FortwistMetrics {
  val MetricHeaders = Vector(
    "foraminiferaCount",
    "algaeCount",
    "foraminiferaDeaths",
    "foraminiferaTotalEnergy",
    "foraminiferaReproductionsCount",
    "consumedAlgaeCount",
    "foraminiferaTotalLifespan",
    "foraminiferaMoves"
  )

  private val Empty = FortwistMetrics(0, 0, 0, 0, 0, 0, 0, 0)

  def empty: FortwistMetrics = Empty
}