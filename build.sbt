import Dependencies._

ThisBuild / scalaVersion     := "3.3.1"
ThisBuild / version          := "0.0.1-SNAPSHOT"
ThisBuild / organization     := "com.gu"
ThisBuild / organizationName := "guardian"

ThisBuild / scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-encoding", "UTF-8",
  "-deprecation",
)

lazy val dynamodecs = (project in file("."))
  .settings(
    name := "dynamodecs",
    libraryDependencies ++= Seq(
      scalatest % Test,
      dynamoDb,
      circeGeneric % Test,
    ) ++ circe,
  )
