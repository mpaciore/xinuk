package pl.edu.agh.xinuk.config

trait XinukConfig {
  def gridSize: Int

  def signalSuppressionFactor: Double

  def shardingMod: Int
}
