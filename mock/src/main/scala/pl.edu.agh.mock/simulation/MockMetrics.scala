package pl.edu.agh.mock.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class MockMetrics() extends Metrics {
  override def log: String = {
    s""
  }

  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): MockMetrics = {
    this
  }
}

object MockMetrics {
  private val EMPTY = MockMetrics()

  def empty(): MockMetrics = EMPTY
}