package pl.edu.agh.mock.utlis

import java.io.FileInputStream

import pl.edu.agh.mock.model
import pl.edu.agh.mock.model.{SimulationMap, Tile, TileType}
import play.api.libs.json.{JsError, JsResult, JsString, JsSuccess, JsValue, Json, Reads}

object JsonMapParser {

  def parseMapFromJson(filename: String): SimulationMap = {
    val stream = new FileInputStream(filename)
    implicit val tileTypeObstacleReader: Reads[TileType.Obstacle.type ] = Json.reads[TileType.Obstacle.type ]
    implicit val tileTypeEmptyReader: Reads[TileType.Empty.type ] = Json.reads[TileType.Empty.type ]
    implicit val tileTypeReader: Reads[TileType.EnumVal] = Json.reads[TileType.EnumVal]
    implicit val tileReader: Reads[Tile] = Json.reads[Tile]
    implicit val mapReader: Reads[SimulationMap] = Json.reads[SimulationMap]

    val jsonMap = try { Json.parse(stream) } finally stream.close()
    Json.fromJson[SimulationMap](jsonMap).get
  }

    private def enumReads[E <: Enumeration](enum: E): Reads[E#Value] = new Reads[E#Value] {
      def reads(json: JsValue): JsResult[E#Value] = json match {
        case JsString(s) => {
          try {
            JsSuccess(enum.withName(s))
          } catch {
            case _: NoSuchElementException => JsError(s"Enumeration expected of type: '${enum.getClass}', but it does not appear to contain the value: '$s'")
          }
        }
        case _ => JsError("String value expected")
      }
    }

}


