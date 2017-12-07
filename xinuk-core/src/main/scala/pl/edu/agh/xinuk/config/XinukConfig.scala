package pl.edu.agh.xinuk.config

trait XinukConfig {
  def gridSize: Int
  def signalSuppressionFactor: Double
  def workersRoot: Int
  def shardingMod: Int

  def signalSpeedRatio: Int

  def iterationsNumber: Long
}
