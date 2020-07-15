package pl.edu.agh.xinuk.model

trait Direction {
  def opposite: Direction

  def adjacent: Seq[Direction]

  def withAdjacent: Seq[Direction] = adjacent :+ this
}
