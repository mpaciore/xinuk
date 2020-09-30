package pl.edu.agh.urban.algorithm

import pl.edu.agh.xinuk.algorithm.Metrics

final case class UrbanMetrics(v1: Long, v2: Long) extends Metrics {
  override def log: String = {
    s"$v1;$v2"
  }

  override def series: Vector[(String, Double)] = Vector(
    "V1" -> v1.toDouble,
    "V2" -> v2.toDouble
  )

  override def +(other: Metrics): UrbanMetrics = other match {
    case UrbanMetrics.EMPTY => this
    case UrbanMetrics(otherV1, otherV2) =>
      UrbanMetrics(v1 + otherV1, v2 + otherV2)
    case null => this
    case _ => throw new UnsupportedOperationException(s"Cannot add non-UrbanMetrics to UrbanMetrics")
  }
}

object UrbanMetrics {
  private val EMPTY = UrbanMetrics(0, 0)

  def empty: UrbanMetrics = EMPTY
}
