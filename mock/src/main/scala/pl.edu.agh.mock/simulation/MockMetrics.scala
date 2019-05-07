package pl.edu.agh.mock.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class MockMetrics(mockPopulation: Int) extends Metrics {
  override def log: String = {
    s"$mockPopulation"
  }

  override def series: Vector[(String, Double)] = Vector(
    "MockPopulation" -> mockPopulation
  )

  override def +(other: Metrics): MockMetrics = {
    other match {
      case MockMetrics.EMPTY => this
      case MockMetrics(otherMockPopulation) =>
        MockMetrics(mockPopulation + otherMockPopulation)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-MockMetrics to MockMetrics")
    }
  }
}

object MockMetrics {
  private val EMPTY = MockMetrics(0)

  def empty(): MockMetrics = EMPTY
}