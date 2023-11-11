ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "CS441CatchMeClient",
    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "32.1.3-jre",
      "org.scalatest" %% "scalatest" % "3.2.15" % "test",
      "com.typesafe" % "config" % "1.4.2",
      "com.softwaremill.sttp.client3" %% "core" % "3.9.1",
      "com.softwaremill.sttp.client3" %% "circe" % "3.9.1",
      "io.circe" %% "circe-generic" % "0.14.5",
      "io.circe" %% "circe-parser" % "0.14.5",

    )
  )


assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}

