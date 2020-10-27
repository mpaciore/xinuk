cancelable in Global := true

val Version = new {
  val Akka = "2.5.31"
  val AkkaKryo = "1.0.0"
  val Logback = "1.2.3"
  val AvsCommons = "2.0.0-M12"
  val ScalaTest = "3.2.2"
  val Mockito = "3.5.10"
  val ScalaLogging = "3.9.2"
  val Ficus = "1.5.0"
  val ScalaSwing = "2.1.1"
  val JFreeChart = "1.5.0"
  val JacksonScala = "2.11.2"
}

inThisBuild(Seq(
  organization := "pl.edu.agh",
  version := "1.1-SNAPSHOT",
  scalaVersion := "2.13.3",

  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics",
    "-language:experimental.macros",
    "-language:higherKinds",
    "-Xfatal-warnings",
    "-Xlint:-missing-interpolator,-adapted-args,-unused,_"
  ),
))

lazy val xinuk = project.in(file("."))
  .aggregate(`xinuk-core`, rabbits, fortwist, torch, mock, urban)
  .disablePlugins(AssemblyPlugin)

lazy val `xinuk-core` = project
  .settings(
    name := "xinuk-core",
    libraryDependencies ++= Seq(
      "com.avsystem.commons" %% "commons-core" % Version.AvsCommons,
      "io.altoo" %% "akka-kryo-serialization" % Version.AkkaKryo,
      "com.iheart" %% "ficus" % Version.Ficus,
      "com.typesafe.akka" %% "akka-actor" % Version.Akka,
      "com.typesafe.akka" %% "akka-slf4j" % Version.Akka,
      "com.typesafe.akka" %% "akka-cluster" % Version.Akka,
      "com.typesafe.akka" %% "akka-cluster-sharding" % Version.Akka,
      "com.typesafe.scala-logging" %% "scala-logging" % Version.ScalaLogging,
      "org.scala-lang.modules" %% "scala-swing" % Version.ScalaSwing,
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % Version.JacksonScala,
      "org.jfree" % "jfreechart" % Version.JFreeChart,
      "org.scalatest" %% "scalatest" % Version.ScalaTest % Test,
      "com.typesafe.akka" %% "akka-testkit" % Version.Akka % Test,
      "org.mockito" % "mockito-core" % Version.Mockito % Test,
    ),
  ).disablePlugins(AssemblyPlugin)

def modelProject(projectName: String)(mainClassName: String): Project = {
  Project(projectName, file(projectName))
    .settings(
      name := projectName,
      libraryDependencies ++= Seq(
        "ch.qos.logback" % "logback-classic" % Version.Logback,
        "org.scalatest" %% "scalatest" % Version.ScalaTest % Test,
        "com.typesafe.akka" %% "akka-testkit" % Version.Akka % Test,
        "org.mockito" % "mockito-core" % Version.Mockito % Test,
      ),
      mainClass in assembly := Some(mainClassName),
      assemblyJarName in assembly := s"$projectName.jar",
      test in assembly := {},
      assemblyMergeStrategy in assembly := {
        case "module-info.class" => MergeStrategy.first
        case x =>
          val oldStrategy = (assemblyMergeStrategy in assembly).value
          oldStrategy(x)
      }
    ).dependsOn(`xinuk-core`)
}

lazy val rabbits = modelProject("rabbits")("pl.edu.agh.rabbits.RabbitsMain")
lazy val fortwist = modelProject("fortwist")("pl.edu.agh.fortwist.FortwistMain")
lazy val torch = modelProject("torch")("pl.edu.agh.torch.TorchMain")
lazy val mock = modelProject("mock")("pl.edu.agh.mock.MockMain")
lazy val urban = modelProject("urban")("pl.edu.agh.urban.UrbanMain")
