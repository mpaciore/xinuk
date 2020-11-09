package pl.edu.agh.urban.algorithm

import pl.edu.agh.xinuk.algorithm.Metrics
import pl.edu.agh.xinuk.model.grid.GridCellId

final case class UrbanMetrics(
                               peopleCreated: Long,
                               peopleRemoved: Long,
                               socialDistanceViolationCount: Long,
                               socialDistanceViolationLocations: Seq[GridCellId]
                             ) extends Metrics {
  override def log: String = {
    s"$peopleCreated;$peopleRemoved;$socialDistanceViolationCount;$socialDistanceViolationLocations"
  }

  override def series: Vector[(String, Double)] = Vector(
    "People entering area" -> peopleCreated.toDouble,
    "People exiting area" -> peopleRemoved.toDouble,
    "Social distance violations" -> socialDistanceViolationCount.toDouble
  )

  override def +(other: Metrics): UrbanMetrics = other match {
    case UrbanMetrics.Empty => this
    case UrbanMetrics(otherPeopleCreated, otherPeopleRemoved, otherSocialDistanceViolationCount,
    otherSocialDistanceViolationLocations) =>
      UrbanMetrics(peopleCreated + otherPeopleCreated,
        peopleRemoved + otherPeopleRemoved,
        socialDistanceViolationCount + otherSocialDistanceViolationCount,
        socialDistanceViolationLocations ++ otherSocialDistanceViolationLocations)
    case _ => throw new UnsupportedOperationException(s"Cannot add non-UrbanMetrics to UrbanMetrics")
  }
}

object UrbanMetrics {
  val MetricHeaders = Vector(
    "peopleCreated",
    "peopleRemoved",
    "socialDistanceViolationCount",
    "socialDistanceViolationLocations"
  )

  private val Empty = UrbanMetrics(0, 0, 0, Seq.empty)
  private val PersonCreated = UrbanMetrics(1, 0, 0, Seq.empty)
  private val PersonRemoved = UrbanMetrics(0, 1, 0, Seq.empty)

  def empty: UrbanMetrics = Empty

  def personCreated: UrbanMetrics = PersonCreated

  def personRemoved: UrbanMetrics = PersonRemoved

  def violation(location: GridCellId): UrbanMetrics = UrbanMetrics(0, 0, 1, Seq(location))
}
