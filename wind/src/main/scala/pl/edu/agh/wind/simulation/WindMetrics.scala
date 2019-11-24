package pl.edu.agh.wind.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class WindMetrics() extends Metrics {
  override def log: String = {
    s""
  }

  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): WindMetrics = {
    this
  }
}

object WindMetrics {
  private val EMPTY = WindMetrics()

  def empty(): WindMetrics = EMPTY
}
