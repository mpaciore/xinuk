package pl.edu.agh.torch.algorithm

import pl.edu.agh.xinuk.algorithm.Metrics

final case class TorchMetrics(peopleCount: Long,
                              fireCount: Long,
                              exitCount: Long,
                              peopleDeaths: Long,
                              peopleEscapes: Long) extends Metrics {
  override def log: String = {
    s"$peopleCount;$fireCount;$exitCount;$peopleDeaths;$peopleEscapes"
  }

  override def series: Vector[(String, Double)] = Vector(
    "people count" -> peopleCount,
    "fire count" -> fireCount,
    "exit count" -> exitCount,
    "people deaths" -> peopleDeaths,
    "people escapes" -> peopleEscapes
  )

  override def +(other: Metrics): TorchMetrics = {
    other match {
      case TorchMetrics.Empty => this
      case TorchMetrics(otherPeopleCount, otherFireCount, otherExitCount, otherPeopleDeaths, otherPeopleEscapes) =>
        TorchMetrics(peopleCount + otherPeopleCount, fireCount + otherFireCount, exitCount + otherExitCount,
          peopleDeaths + otherPeopleDeaths, peopleEscapes + otherPeopleEscapes)
      case _ => throw new UnsupportedOperationException(s"Cannot add: non-TorchMetrics to TorchMetrics")
    }
  }
}

object TorchMetrics {
  private val Empty = TorchMetrics(0, 0, 0, 0, 0)
  private val Person = TorchMetrics(1, 0, 0, 0, 0)
  private val Fire = TorchMetrics(0, 1, 0, 0, 0)
  private val Exit = TorchMetrics(0, 0, 1, 0, 0)
  private val Death = TorchMetrics(0, 0, 0, 1, 0)
  private val Escape = TorchMetrics(0, 0, 0, 0, 1)

  def empty: TorchMetrics = Empty

  def person: TorchMetrics = Person

  def fire: TorchMetrics = Fire

  def exit: TorchMetrics = Exit

  def death: TorchMetrics = Death

  def escape: TorchMetrics = Escape
}
