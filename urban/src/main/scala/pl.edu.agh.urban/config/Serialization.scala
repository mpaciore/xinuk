package pl.edu.agh.urban.config

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Path, Paths}
import java.time.LocalTime
import java.time.format.DateTimeFormatter

import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.core.{JsonGenerator, JsonParser}
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import javax.imageio.ImageIO
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridDirection}
import pl.edu.agh.xinuk.model.{Direction, Signal, SignalMap, WorkerId}


object Serialization {
  private val formatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME

  private val mapper: ObjectMapper = {
    val mapper = new ObjectMapper()
    mapper.registerModule(DefaultScalaModule)

    val module = new SimpleModule()

    module.addDeserializer(classOf[Color], ColorDeserializer)
    module.addDeserializer(classOf[Coordinates], CoordinatesDeserializer)

    module.addDeserializer(classOf[TileTypeId], TileTypeIdDeserializer)
    module.addDeserializer(classOf[TargetType], TargetTypeDeserializer)
    module.addDeserializer(classOf[TimeOfDay], TimeOfDayDeserializer)

    module.addSerializer(classOf[GridCellId], GridCellIdSerializer)
    module.addDeserializer(classOf[GridCellId], GridCellIdDeserializer)
    module.addKeySerializer(classOf[GridCellId], GridCellIdKeySerializer)
    module.addKeyDeserializer(classOf[GridCellId], GridCellIdKeyDeserializer)

    module.addSerializer(classOf[Direction], DirectionSerializer)
    module.addDeserializer(classOf[Direction], DirectionDeserializer)
    module.addKeySerializer(classOf[Direction], DirectionKeySerializer)
    module.addKeyDeserializer(classOf[Direction], DirectionKeyDeserializer)

    module.addSerializer(classOf[Signal], SignalSerializer)
    module.addDeserializer(classOf[Signal], SignalDeserializer)

    module.addSerializer(classOf[LocalTime], LocalTimeSerializer)
    module.addDeserializer(classOf[LocalTime], LocalTimeDeserializer)

    mapper.registerModule(module)

    mapper
  }

  def loadMapImage()(implicit config: UrbanConfig): BufferedImage = {
    val path = Paths.get(config.urbanDataRootPath, config.mapImageFilename)
    ImageIO.read(path.toFile)
  }

  def loadTileTypes()(implicit config: UrbanConfig): Seq[TileType] = {
    val path = Paths.get(config.urbanDataRootPath, config.tileTypesFilename)
    mapper.readValue(path.toFile, new TypeReference[Seq[TileType]]() {})
  }

  def loadTargets()(implicit config: UrbanConfig): Seq[TargetInfo] = {
    val path = Paths.get(config.urbanDataRootPath, config.targetsFilename)
    mapper.readValue(path.toFile, new TypeReference[Seq[TargetInfo]]() {})
  }

  def loadPersonBehavior()(implicit config: UrbanConfig): PersonBehavior = {
    val path = Paths.get(config.urbanDataRootPath, config.personBehaviorFilename)
    mapper.readValue(path.toFile, new TypeReference[PersonBehavior]() {})
  }

  def loadStaticSignal()(implicit config: UrbanConfig): Map[String, Map[GridCellId, SignalMap]] = {
    val path: Path = Paths.get(config.urbanDataRootPath, config.staticSignalDir)
    path.toFile.list().map {
//    List("BB01").map { // TODO remove
        buildingId =>
          val buildingPath = Paths.get(path.toString, buildingId)
          val buildingSignal = buildingPath.toFile.list().map {
            filename => readSignalFile(Paths.get(buildingPath.toString, filename).toFile).toSeq
          }.reduce(_ ++ _)
            .toMap
          (buildingId, buildingSignal)
      }.toMap
  }

  private def readSignalFile(file: File): Map[GridCellId, SignalMap] =
    mapper.readValue(file, new TypeReference[Map[GridCellId, SignalMap]]() {})

  def dumpStaticSignal(signal: Map[GridCellId, SignalMap], buildingId: String, workerId: WorkerId)(implicit config: UrbanConfig): Unit = {
    val path = Paths.get(config.urbanDataRootPath, config.staticSignalDir, buildingId, f"${workerId.value}%04d.json")
    path.getParent.toFile.mkdirs()
    path.toFile.createNewFile()
    writeSignalFile(signal, path.toFile)
  }

  private def writeSignalFile(signal: Map[GridCellId, SignalMap], file: File): Unit =
    mapper.writeValue(file, signal)

  private object ColorDeserializer extends JsonDeserializer[Color] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): Color = {
      val node: JsonNode = p.getCodec.readTree(p)
      val r = node.get(0).asInt
      val g = node.get(1).asInt
      val b = node.get(2).asInt
      new Color(r, g, b)
    }
  }

  private object CoordinatesDeserializer extends JsonDeserializer[Coordinates] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): Coordinates = {
      val node: JsonNode = p.getCodec.readTree(p)
      val x = node.get(0).asInt
      val y = node.get(1).asInt
      Coordinates(x, y)
    }
  }

  private object TileTypeIdDeserializer extends JsonDeserializer[TileTypeId] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): TileTypeId = {
      val node: JsonNode = p.getCodec.readTree(p)
      val value = node.textValue()
      TileTypeId.values.find(_.value == value).get
    }
  }

  private object TargetTypeDeserializer extends JsonDeserializer[TargetType] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): TargetType = {
      val node: JsonNode = p.getCodec.readTree(p)
      val value = node.textValue()
      TargetType.values.find(_.value == value).get
    }
  }

  private object TimeOfDayDeserializer extends JsonDeserializer[TimeOfDay] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): TimeOfDay = {
      val node: JsonNode = p.getCodec.readTree(p)
      val value = node.textValue()
      TimeOfDay.values.find(_.value == value).get
    }
  }

  private object GridCellIdSerializer extends JsonSerializer[GridCellId] {
    override def serialize(id: GridCellId, json: JsonGenerator, provider: SerializerProvider): Unit = {
      json.writeString(s"${id.x},${id.y}")
    }
  }

  private object GridCellIdDeserializer extends JsonDeserializer[GridCellId] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): GridCellId = {
      val node: JsonNode = p.getCodec.readTree(p)
      val Seq(x, y) = node.asText.split(",").toSeq
      GridCellId(x.toInt, y.toInt)
    }
  }

  private object GridCellIdKeySerializer extends JsonSerializer[GridCellId] {
    override def serialize(id: GridCellId, json: JsonGenerator, provider: SerializerProvider): Unit = {
      json.writeFieldName(s"${id.x},${id.y}")
    }
  }

  private object GridCellIdKeyDeserializer extends KeyDeserializer {
    override def deserializeKey(key: String, ctxt: DeserializationContext): GridCellId = {
      val Seq(x, y) = key.split(",").toSeq
      GridCellId(x.toInt, y.toInt)
    }
  }

  private object DirectionSerializer extends JsonSerializer[Direction] {
    override def serialize(direction: Direction, json: JsonGenerator, provider: SerializerProvider): Unit = {
      json.writeString(direction.toString)
    }
  }

  private object DirectionDeserializer extends JsonDeserializer[Direction] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): Direction = {
      val node: JsonNode = p.getCodec.readTree(p)
      val value = node.textValue()
      GridDirection.values.find(_.toString == value).get
    }
  }

  private object DirectionKeySerializer extends JsonSerializer[Direction] {
    override def serialize(direction: Direction, json: JsonGenerator, provider: SerializerProvider): Unit = {
      json.writeFieldName(direction.toString)
    }
  }

  private object DirectionKeyDeserializer extends KeyDeserializer {
    override def deserializeKey(key: String, ctxt: DeserializationContext): Direction = {
      GridDirection.values.find(_.toString == key).get
    }
  }

  private object SignalSerializer extends JsonSerializer[Signal] {
    override def serialize(id: Signal, json: JsonGenerator, provider: SerializerProvider): Unit = {
      json.writeNumber(id.value)
    }
  }

  private object SignalDeserializer extends JsonDeserializer[Signal] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): Signal = {
      import com.fasterxml.jackson.databind.JsonNode
      val value = p.getCodec.readTree[JsonNode](p).asDouble()
      Signal(value)
    }
  }

  private object LocalTimeSerializer extends JsonSerializer[LocalTime] {
    override def serialize(time: LocalTime, json: JsonGenerator, provider: SerializerProvider): Unit = {
      json.writeString(time.format(formatter))
    }
  }

  private object LocalTimeDeserializer extends JsonDeserializer[LocalTime] {
    override def deserialize(p: JsonParser, ctxt: DeserializationContext): LocalTime = {
      import com.fasterxml.jackson.databind.JsonNode
      val value = p.getCodec.readTree[JsonNode](p).textValue()
      LocalTime.parse(value, formatter)
    }
  }
}
