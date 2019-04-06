package pl.edu.agh.school.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class SchoolMetrics(foraminiferaCount: Long,
                               algaeCount: Long,
                               foraminiferaDeaths: Long,
                               foraminiferaTotalEnergy: Double,
                               foraminiferaReproductionsCount: Long,
                               consumedAlgaeCount: Long,
                               foraminiferaTotalLifespan: Long,
                               algaeTotalLifespan: Long) extends Metrics {

  override def log: String = {
    s"$foraminiferaCount;$algaeCount;$foraminiferaDeaths;$foraminiferaTotalEnergy;$foraminiferaReproductionsCount;$consumedAlgaeCount;$foraminiferaTotalLifespan;$algaeTotalLifespan"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Foraminifera" -> foraminiferaCount,
    "Algae" -> algaeCount
  )

  override def +(other: Metrics): SchoolMetrics = {
    other match {
      case SchoolMetrics.EMPTY => this
      case SchoolMetrics(otherForaminiferaCount, otherAlgaeCount, otherForaminiferaDeaths, otherForaminiferaTotalEnergy,
      otherForaminiferaReproductionsCount, otherConsumedAlgaeCount, otherForaminiferaTotalLifespan,
      otherAlgaeTotalLifespan) =>
        SchoolMetrics(foraminiferaCount + otherForaminiferaCount, algaeCount + otherAlgaeCount,
          foraminiferaDeaths + otherForaminiferaDeaths, foraminiferaTotalEnergy + otherForaminiferaTotalEnergy,
          foraminiferaReproductionsCount + otherForaminiferaReproductionsCount,
          consumedAlgaeCount + otherConsumedAlgaeCount, foraminiferaTotalLifespan + otherForaminiferaTotalLifespan,
          algaeTotalLifespan + otherAlgaeTotalLifespan)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-ForminMetrics to ForminMetrics")
    }
  }
}

object SchoolMetrics {
  private val EMPTY = SchoolMetrics(0, 0, 0, 0, 0, 0, 0, 0)

  def empty(): SchoolMetrics = EMPTY
}