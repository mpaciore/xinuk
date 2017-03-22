name := "formin"

version := "1.0"

scalaVersion := "2.11.8"
organization := "pl.edu.agh"

cancelable in Global := true

lazy val Version = new {
  lazy val Akka = "2.4.17"
  lazy val Logback = "1.2.1"
  lazy val Guava = "21.0"
  lazy val AvsCommons = "1.19.10"
  lazy val ScalaTest = "3.0.1"
  lazy val ScalaLogging = "3.5.0"
  lazy val Ficus = "1.4.0"
  lazy val ScalaSwing = "2.0.0"
  lazy val JFreeChart = "1.0.19"
}

dependencyOverrides ++= Set(
  "com.google.guava" % "guava" % Version.Guava
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % Version.Akka,
  "com.typesafe.akka" %% "akka-slf4j" % Version.Akka,
  "ch.qos.logback" % "logback-classic" % Version.Logback,
  "com.google.guava" % "guava" % Version.Guava,
  "com.avsystem.commons" %% "commons-core" % Version.AvsCommons,
  "com.typesafe.scala-logging" %% "scala-logging" % Version.ScalaLogging,
  "com.iheart" %% "ficus" % Version.Ficus,
  "org.scalatest" %% "scalatest" % Version.ScalaTest % Test,
  "com.typesafe.akka" %% "akka-testkit" % Version.Akka % Test,
  "org.scala-lang.modules" %% "scala-swing" % Version.ScalaSwing,
  "org.jfree" % "jfreechart" % Version.JFreeChart
)

