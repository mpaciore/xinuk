package pl.edu.agh.xinuk.simulation

trait Metrics {
  def log: String

  override final def toString: String = log
}
