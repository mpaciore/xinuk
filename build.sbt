name := "formin"
organization := "pl.edu.agh"

version := "1.0"

scalaVersion := "2.11.11"
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-Xfuture",
  "-Xfatal-warnings",
  "-Xlint:_,-missing-interpolator,-adapted-args"
)

cancelable in Global := true

lazy val Version = new {
  lazy val Akka = "2.5.1"
  lazy val Logback = "1.2.3"
  lazy val Guava = "21.0"
  lazy val AvsCommons = "1.20.4"
  lazy val ScalaTest = "3.0.3"
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
  "com.typesafe.akka" %% "akka-cluster" % Version.Akka,
  "com.typesafe.akka" %% "akka-cluster-sharding" % Version.Akka,
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

