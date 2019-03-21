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

  override def +(other: Metrics): TorchMetrics = {
    other match {
      case TorchMetrics.EMPTY => this
      case TorchMetrics(otherPeopleCount, otherFireCount, otherEscapeCount, otherPeopleDeaths, otherPeopleEscaped) =>
        TorchMetrics(peopleCount + otherPeopleCount, fireCount + otherFireCount, escapeCount + otherEscapeCount,
          peopleDeaths + otherPeopleDeaths, peopleEscaped + otherPeopleEscaped)
      case null => this
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-TorchMetrics to TorchMetrics")
    }
  }
}

object TorchMetrics {
  private val EMPTY = TorchMetrics(0, 0, 0, 0, 0)

  def empty(): TorchMetrics = EMPTY
}
