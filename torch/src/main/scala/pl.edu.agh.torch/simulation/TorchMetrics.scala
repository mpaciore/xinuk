package pl.edu.agh.torch.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class TorchMetrics(peopleCount: Long,
                              fireCount: Long,
                              escapeCount: Long,
                              peopleDeaths: Long) extends Metrics {
  override def log: String = {
    s"$peopleCount;$fireCount;$peopleDeaths"
  }

  override def series: Vector[(String, Double)] = Vector(
    "People" -> peopleCount,
    "Fire" -> fireCount,
    "Escape" -> escapeCount
  )
}
