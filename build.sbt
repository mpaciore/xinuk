cancelable in Global := true

val Version = new {
  val Akka = "2.5.6"
  val AkkaKryo = "0.5.2"
  val Logback = "1.2.3"
  val Guava = "23.0"
  val AvsCommons = "1.24.0"
  val ScalaTest = "3.0.4"
  val ScalaLogging = "3.5.0"
  val Ficus = "1.4.3"
  val ScalaSwing = "2.0.1"
  val JFreeChart = "1.0.19"
}

inThisBuild(Seq(
  name := "formin",
  organization := "pl.edu.agh",
  version := "1.1-SNAPSHOT",
  scalaVersion := "2.12.4",
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
  ),
))

lazy val formin = project.in(file("formin"))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor" % Version.Akka,
      "com.typesafe.akka" %% "akka-slf4j" % Version.Akka,
      "com.typesafe.akka" %% "akka-cluster" % Version.Akka,
      "com.typesafe.akka" %% "akka-cluster-sharding" % Version.Akka,
      "com.github.romix.akka" %% "akka-kryo-serialization" % Version.AkkaKryo,
      "ch.qos.logback" % "logback-classic" % Version.Logback,
      "com.google.guava" % "guava" % Version.Guava,
      "com.avsystem.commons" %% "commons-core" % Version.AvsCommons,
      "com.typesafe.scala-logging" %% "scala-logging" % Version.ScalaLogging,
      "com.iheart" %% "ficus" % Version.Ficus,
      "org.scala-lang.modules" %% "scala-swing" % Version.ScalaSwing,
      "org.jfree" % "jfreechart" % Version.JFreeChart,
      "org.scalatest" %% "scalatest" % Version.ScalaTest % Test,
      "com.typesafe.akka" %% "akka-testkit" % Version.Akka % Test,
    ),
    mainClass in assembly := Some("pl.edu.agh.formin.Simulation"),
    assemblyJarName in assembly := "formin.jar",
    test in assembly := {},
  )



