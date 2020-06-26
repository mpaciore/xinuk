package pl.edu.agh.mock.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class MockMetrics(mockCount: Long, mockMoves: Long) extends Metrics {
  override def log: String = {
    s"$mockCount;$mockMoves"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Count" -> mockCount,
    "Moves" -> mockMoves
  )

  override def +(other: Metrics): MockMetrics = other match {
    case MockMetrics.EMPTY => this
    case MockMetrics(otherMockCount, otherMockMoves) =>
      MockMetrics(mockCount + otherMockCount, mockMoves + otherMockMoves)
    case null => this
    case _ => throw new UnsupportedOperationException(s"Cannot add non-MockMetrics to MockMetrics")
  }
}

object MockMetrics {
  private val EMPTY = MockMetrics(0, 0)

  def empty: MockMetrics = EMPTY
}
