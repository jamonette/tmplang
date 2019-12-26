import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "tmplang",
    fork in run := true,
    outputStrategy in run := Some(StdoutOutput),
    connectInput in run := true,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.typelevel" %% "cats-core" % "2.0.0",
      "org.typelevel" %% "cats-effect" % "2.0.0")
  )

