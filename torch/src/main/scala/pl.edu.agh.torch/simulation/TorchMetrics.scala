package pl.edu.agh.torch.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class TorchMetrics(peopleCount: Long,
                              fireCount: Long,
                              escapeCount: Long,
                              peopleDeaths: Long,
                              peopleEscaped: Long) extends Metrics {
  override def log: String = {
    s"$peopleCount;$fireCount;$escapeCount;$peopleDeaths;$peopleEscaped"
  }

  override def series: Vector[(String, Double)] = Vector(
    "People" -> peopleCount,
    "Fire" -> fireCount,
    "Escape" -> escapeCount,
    "PeopleDeaths" -> peopleDeaths
  )
}
