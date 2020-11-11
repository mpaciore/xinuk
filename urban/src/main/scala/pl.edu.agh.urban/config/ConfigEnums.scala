package pl.edu.agh.urban.config

import com.fasterxml.jackson.annotation.JsonValue

import scala.annotation.meta.getter

case class TileTypeId(@(JsonValue@getter) value: String) extends AnyVal

object TileTypeId {
  val None: TileTypeId = TileTypeId("None")

  val BuildingResidential: TileTypeId = TileTypeId("BuildingResidential")

  val BuildingSocial: TileTypeId = TileTypeId("BuildingSocial")

  val BuildingCommercial: TileTypeId = TileTypeId("BuildingCommercial")

  val BuildingOffice: TileTypeId = TileTypeId("BuildingOffice")

  val BuildingHybrid: TileTypeId = TileTypeId("BuildingHybrid")

  val BuildingAuxiliary: TileTypeId = TileTypeId("BuildingAuxiliary")

  val Garage: TileTypeId = TileTypeId("Garage")

  val SportField: TileTypeId = TileTypeId("SportField")

  val SidewalkCity: TileTypeId = TileTypeId("SidewalkCity")

  val SidewalkResidential: TileTypeId = TileTypeId("SidewalkResidential")

  val RoadAccess: TileTypeId = TileTypeId("RoadAccess")

  val Road: TileTypeId = TileTypeId("Road")

  val Wall: TileTypeId = TileTypeId("Wall")

  val Barren: TileTypeId = TileTypeId("Barren")

  val Garden: TileTypeId = TileTypeId("Garden")

  val CafeGarden: TileTypeId = TileTypeId("CafeGarden")

  val Square: TileTypeId = TileTypeId("Square")

  val Park: TileTypeId = TileTypeId("Park")

  val ParkingBuilding: TileTypeId = TileTypeId("ParkingBuilding")

  val ParkingService: TileTypeId = TileTypeId("ParkingService")

  val ParkingCollective: TileTypeId = TileTypeId("ParkingCollective")

  val StoreYard: TileTypeId = TileTypeId("StoreYard")

  val Playground: TileTypeId = TileTypeId("Playground")

  val Crossing: TileTypeId = TileTypeId("Crossing")

  val Stop: TileTypeId = TileTypeId("Stop")

  val SquareResidential: TileTypeId = TileTypeId("SquareResidential")

  val Path: TileTypeId = TileTypeId("Path")

  val PathCyclist: TileTypeId = TileTypeId("PathCyclist")

  val PrivateArea: TileTypeId = TileTypeId("PrivateArea")

  val Backstage: TileTypeId = TileTypeId("Backstage")

  val SchoolArea: TileTypeId = TileTypeId("SchoolArea")

  val Tracks: TileTypeId = TileTypeId("Tracks")

  val BuildingEntrance: TileTypeId = TileTypeId("BuildingEntrance")

  val GarageEntrance: TileTypeId = TileTypeId("GarageEntrance")

  val GreeneryLow: TileTypeId = TileTypeId("GreeneryLow")

  val GreeneryLowResidential: TileTypeId = TileTypeId("GreeneryLowResidential")

  val GreeneryTall: TileTypeId = TileTypeId("GreeneryTall")

  val GreeneryTallResidential: TileTypeId = TileTypeId("GreeneryTallResidential")

  val GreeneryTallRoad: TileTypeId = TileTypeId("GreeneryTallRoad")

  val GreenerySchool: TileTypeId = TileTypeId("GreenerySchool")
  
  val values: Seq[TileTypeId] = Seq(TileTypeId.None, TileTypeId.BuildingResidential, TileTypeId.BuildingSocial,
    TileTypeId.BuildingCommercial, TileTypeId.BuildingOffice, TileTypeId.BuildingHybrid, TileTypeId.BuildingAuxiliary,
    TileTypeId.Garage, TileTypeId.SportField, TileTypeId.SidewalkCity, TileTypeId.SidewalkResidential,
    TileTypeId.RoadAccess, TileTypeId.Road, TileTypeId.Wall, TileTypeId.Barren, TileTypeId.Garden,
    TileTypeId.CafeGarden, TileTypeId.Square, TileTypeId.Park, TileTypeId.ParkingBuilding, TileTypeId.ParkingService,
    TileTypeId.ParkingCollective, TileTypeId.StoreYard, TileTypeId.Playground, TileTypeId.Crossing, TileTypeId.Stop,
    TileTypeId.SquareResidential, TileTypeId.Path, TileTypeId.PathCyclist, TileTypeId.PrivateArea, TileTypeId.Backstage,
    TileTypeId.SchoolArea, TileTypeId.Tracks, TileTypeId.BuildingEntrance, TileTypeId.GarageEntrance,
    TileTypeId.GreeneryLow, TileTypeId.GreeneryLowResidential, TileTypeId.GreeneryTall,
    TileTypeId.GreeneryTallResidential, TileTypeId.GreeneryTallRoad, TileTypeId.GreenerySchool)
}

case class TargetType(@(JsonValue@getter) value: String) extends AnyVal

object TargetType {
  val Parking: TargetType = TargetType("Parking")

  val Bike: TargetType = TargetType("Bike")

  val Bus: TargetType = TargetType("Bus")

  val Outside: TargetType = TargetType("Outside")

  val Playground: TargetType = TargetType("Playground")

  val Wander: TargetType = TargetType("Wander")

  val Residential: TargetType = TargetType("Residential")

  val Service: TargetType = TargetType("Service")

  val Social: TargetType = TargetType("Social")

  val values: Seq[TargetType] = Seq(TargetType.Parking, TargetType.Bus, TargetType.Outside, TargetType.Playground,
    TargetType.Wander, TargetType.Residential, TargetType.Service, TargetType.Social)
}

case class TimeOfDay(@(JsonValue@getter) value: String) extends AnyVal

object TimeOfDay {
  val Morning: TimeOfDay = TimeOfDay("Morning")

  val Midday: TimeOfDay = TimeOfDay("Midday")

  val Afternoon: TimeOfDay = TimeOfDay("Afternoon")

  val Evening: TimeOfDay = TimeOfDay("Evening")

  val values: Seq[TimeOfDay] = Seq(TimeOfDay.Morning, TimeOfDay.Midday, TimeOfDay.Afternoon, TimeOfDay.Evening)
}
