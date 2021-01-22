package pl.edu.agh.rabbits.algorithm

import pl.edu.agh.rabbits.model.{Lettuce, Rabbit}
import pl.edu.agh.xinuk.algorithm.Metrics

final case class RabbitsMetrics(rabbitCount: Long,
                                lettuceCount: Long,
                                rabbitDeaths: Long,
                                rabbitTotalEnergy: Double,
                                rabbitReproductionsCount: Long,
                                consumedLettuceCount: Long,
                                rabbitTotalLifespan: Long,
                                lettuceTotalLifespan: Long) extends Metrics {

  override def log: String = {
    s"$rabbitCount;$lettuceCount;$rabbitDeaths;$rabbitTotalEnergy;$rabbitReproductionsCount;$consumedLettuceCount;$rabbitTotalLifespan;$lettuceTotalLifespan"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Rabbits" -> rabbitCount.toDouble,
    "Lettuce" -> lettuceCount.toDouble
  )

  override def +(other: Metrics): RabbitsMetrics = {
    other match {
      case RabbitsMetrics.Empty => this
      case RabbitsMetrics(otherRabbitCount, otherLettuceCount, otherRabbitDeaths, otherRabbitTotalEnergy,
      otherRabbitReproductionsCount, otherConsumedLettuceCount, otherRabbitTotalLifespan, otherLettuceTotalLifespan) =>
        RabbitsMetrics(
          rabbitCount + otherRabbitCount,
          lettuceCount + otherLettuceCount,
          rabbitDeaths + otherRabbitDeaths,
          rabbitTotalEnergy + otherRabbitTotalEnergy,
          rabbitReproductionsCount + otherRabbitReproductionsCount,
          consumedLettuceCount + otherConsumedLettuceCount,
          rabbitTotalLifespan + otherRabbitTotalLifespan,
          lettuceTotalLifespan + otherLettuceTotalLifespan)
      case _ => throw new UnsupportedOperationException(s"Cannot add non-RabbitsMetrics to RabbitsMetrics")
    }
  }
}

object RabbitsMetrics {
  val MetricHeaders = Vector(
    "rabbitCount",
    "lettuceCount",
    "rabbitDeaths",
    "rabbitTotalEnergy",
    "rabbitReproductionsCount",
    "consumedLettuceCount",
    "rabbitTotalLifespan",
    "lettuceTotalLifespan"
  )

  private val Empty = RabbitsMetrics(0, 0, 0, 0, 0, 0, 0, 0)
  private val Lettuce = RabbitsMetrics(0, 1, 0, 0, 0, 0, 0, 0)
  private val RabbitReproduction = RabbitsMetrics(0, 0, 0, 0, 1, 0, 0, 0)

  def empty: RabbitsMetrics = Empty
  def rabbit(rabbit: Rabbit): RabbitsMetrics = RabbitsMetrics(1, 0, 0, rabbit.energy, 0, 0, 0, 0)
  def rabbitAddedEnergy(energy: Double): RabbitsMetrics = RabbitsMetrics(0, 0, 0, energy, 0, 0, 0, 0)
  def lettuce: RabbitsMetrics = Lettuce
  def rabbitDeath(rabbit: Rabbit): RabbitsMetrics = RabbitsMetrics(0, 0, 1, 0, 0, 0, rabbit.lifespan, 0)
  def rabbitReproduction: RabbitsMetrics = RabbitReproduction
  def lettuceConsumed(lettuce: Lettuce): RabbitsMetrics = RabbitsMetrics(0, 0, 0, 0, 0, 1, 0, lettuce.lifespan)
}