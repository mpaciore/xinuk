package pl.edu.agh.mock

import java.io.PrintWriter

import pl.edu.agh.mock.model.{SimulationMap, Tile, TileType}
import pl.edu.agh.mock.utlis.{JsonMapParser, SimpleJsonMapGenerator}
import pl.edu.agh.xinuk.model.{EmptyCell, Obstacle}
import play.api.libs.json.{Json, OWrites, Reads}

object TestMain {
  def main(args: Array[String]): Unit = {
    val map = SimpleJsonMapGenerator.randomMap(32)
    implicit val tileTypeObstacleReader: OWrites[TileType.Obstacle.type ] = Json.writes[TileType.Obstacle.type ]
    implicit val tileTypeEmptyReader: OWrites[TileType.Empty.type ] = Json.writes[TileType.Empty.type ]
    implicit val tileTypeReader: OWrites[TileType.EnumVal] = Json.writes[TileType.EnumVal]
    implicit val tileWriter: OWrites[Tile] = Json.writes[Tile]
    implicit val mapWriter: OWrites[SimulationMap] = Json.writes[SimulationMap]
    val json = Json.toJson(map)
    val jsonString = Json.stringify(json)
    new PrintWriter("map.json") { write(jsonString); close() }

    println(JsonMapParser.parseMapFromJson("map.json"))

  }
}
