package pl.edu.agh.mock.model

object TileType {
  sealed trait EnumVal
  case object Empty extends EnumVal
  case object Obstacle extends EnumVal
  val tileTypes: Seq[EnumVal] = Seq(Empty, Obstacle)
}
