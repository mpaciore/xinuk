package pl.edu.agh.urban.algorithm

import pl.edu.agh.xinuk.algorithm.Metrics
import pl.edu.agh.xinuk.model.grid.GridCellId

final case class UrbanMetrics(
                               travelBeginnings: Long,
                               travelEnds: Long,
                               wanderBeginnings: Long,
                               wanderEnds: Long,
                               returnBeginnings: Long,
                               returnEnds: Long,
                               socialDistanceViolationCount: Long,
                               socialDistanceViolationLocations: Seq[GridCellId]
                             ) extends Metrics {
  override def log: String = {
    s"$travelBeginnings;$travelEnds;$wanderBeginnings;$wanderEnds;$returnBeginnings;$returnEnds;$socialDistanceViolationCount;$socialDistanceViolationLocations"
  }

  override def series: Vector[(String, Double)] = Vector(
    "People entering area" -> (travelBeginnings + wanderBeginnings + returnBeginnings).toDouble,
    "People exiting area" -> (travelEnds + wanderEnds + returnEnds).toDouble,
    "Social distance violations" -> socialDistanceViolationCount.toDouble
  )

  override def +(other: Metrics): UrbanMetrics = other match {
    case UrbanMetrics.Empty => this
    case UrbanMetrics(otherTravelBeginnings, otherTravelEnds, otherWanderBeginnings, otherWanderEnds, otherReturnBeginnings, otherReturnEnds,
    otherSocialDistanceViolationCount, otherSocialDistanceViolationLocations) =>
      UrbanMetrics(travelBeginnings + otherTravelBeginnings,
        travelEnds + otherTravelEnds,
        wanderBeginnings + otherWanderBeginnings,
        wanderEnds + otherWanderEnds,
        returnBeginnings + otherReturnBeginnings,
        returnEnds + otherReturnEnds,
        socialDistanceViolationCount + otherSocialDistanceViolationCount,
        socialDistanceViolationLocations ++ otherSocialDistanceViolationLocations)
    case _ => throw new UnsupportedOperationException(s"Cannot add non-UrbanMetrics to UrbanMetrics")
  }
}

object UrbanMetrics {
  val MetricHeaders = Vector(
    "travelBeginnings",
    "travelEnds",
    "wanderBeginnings",
    "wanderEnds",
    "returnBeginnings",
    "returnEnds",
    "socialDistanceViolationCount",
    "socialDistanceViolationLocations"
  )

  private val Empty = UrbanMetrics(0, 0, 0, 0, 0, 0, 0, Seq.empty)
  private val TravelBeginning = UrbanMetrics(1, 0, 0, 0, 0, 0, 0, Seq.empty)
  private val TravelEnd = UrbanMetrics(0, 1, 0, 0, 0, 0, 0, Seq.empty)
  private val WanderBeginning = UrbanMetrics(0, 0, 1, 0, 0, 0, 0, Seq.empty)
  private val WanderEnd = UrbanMetrics(0, 0, 0, 1, 0, 0, 0, Seq.empty)
  private val ReturnBeginning = UrbanMetrics(0, 0, 0, 0, 1, 0, 0, Seq.empty)
  private val ReturnEnd = UrbanMetrics(0, 0, 0, 0, 0, 1, 0, Seq.empty)

  def empty: UrbanMetrics = Empty

  def travelBeginning: UrbanMetrics = TravelBeginning

  def travelEnd: UrbanMetrics = TravelEnd

  def wanderBeginning: UrbanMetrics = WanderBeginning

  def wanderEnd: UrbanMetrics = WanderEnd

  def returnBeginning: UrbanMetrics = ReturnBeginning

  def returnEnd: UrbanMetrics = ReturnEnd

  def violation(location: GridCellId): UrbanMetrics = UrbanMetrics(0, 0, 0, 0, 0, 0, 1, Seq(location))
}
