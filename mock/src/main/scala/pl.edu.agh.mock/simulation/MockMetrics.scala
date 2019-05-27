package pl.edu.agh.mock.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class MockMetrics(
                              mockPopulation: Int,
                              crowdOnProcessor: Int,
                              crowdOnSeams: Int
                            ) extends Metrics {
  override def log: String = {
    s"$mockPopulation;$crowdOnProcessor;$crowdOnSeams"
  }

  override def series: Vector[(String, Double)] = Vector(
    "MockPopulation" -> mockPopulation,
    "CrowdOnProcessor" -> crowdOnProcessor,
    "CrowdOnSeams" -> crowdOnSeams
  )

  override def +(other: Metrics): MockMetrics = {
    other match {
      case MockMetrics.EMPTY => this
      case MockMetrics(otherMockPopulation, otherCrowdOnProcessor, otherCrowdOnSeams) =>
        MockMetrics(
          mockPopulation + otherMockPopulation,
          crowdOnProcessor + otherCrowdOnProcessor,
          crowdOnSeams + otherCrowdOnSeams
        )
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-MockMetrics to MockMetrics")
    }
  }
}

object MockMetrics {
  private val EMPTY = MockMetrics(0, 0, 0)

  def empty(): MockMetrics = EMPTY
}