package pl.edu.agh.urban.model

import java.util.UUID

import pl.edu.agh.urban.config.UrbanConfig

case class Person(var target: Option[String] = Option.empty) {
  val id: String = UUID.randomUUID().toString
  def createPersonMarker(implicit config: UrbanConfig): PersonMarker = PersonMarker(id, 0 /*TODO config.personMarkerInitialTtl*/)
}

case class PersonMarker(id: String, var ttl: Int)
