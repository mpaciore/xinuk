package pl.edu.agh.formin.simulation

import pl.edu.agh.xinuk.simulation.Metrics


final case class ForminMetrics(foraminiferaCount: Long,
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
}