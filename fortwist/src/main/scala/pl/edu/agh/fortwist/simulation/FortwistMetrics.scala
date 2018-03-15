package pl.edu.agh.fortwist.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class FortwistMetrics(
  foraminiferaCount: Long,
  algaeCount: Double,
  foraminiferaDeaths: Long,
  foraminiferaTotalEnergy: Double,
  foraminiferaReproductionsCount: Long,
  consumedAlgaeCount: Double,
  foraminiferaTotalLifespan: Long) extends Metrics {

  override def log: String = {
    s"$foraminiferaCount;$algaeCount;$foraminiferaDeaths;$foraminiferaTotalEnergy;$foraminiferaReproductionsCount;$consumedAlgaeCount;$foraminiferaTotalLifespan"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Foraminifera" -> foraminiferaCount,
    "Algae" -> algaeCount
  )
}