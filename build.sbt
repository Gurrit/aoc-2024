ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"
libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M6"

lazy val aoc2024 = (project in file(s"./day"))
  .settings(
    name := s"Aoc-2024"
  )