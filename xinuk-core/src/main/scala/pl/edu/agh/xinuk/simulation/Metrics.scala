package pl.edu.agh.xinuk.simulation

trait Metrics {
  def log: String

  def series: Vector[(String, Double)]

  def +(other: Metrics): Metrics

  override final def toString: String = log
}
