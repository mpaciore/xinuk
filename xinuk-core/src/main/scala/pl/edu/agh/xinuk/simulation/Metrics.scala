package pl.edu.agh.xinuk.simulation

trait Metrics {
  def log: String
  def series: Vector[(String, Double)]

  override final def toString: String = log
}
