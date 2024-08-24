import Dependencies.*
import scala.concurrent.duration.*

ThisBuild / scalaVersion     := "3.3.1"
ThisBuild / version          := "0.0.1-SNAPSHOT"
ThisBuild / organization     := "com.adamnfish"
ThisBuild / organizationName := "guardian"

ThisBuild / scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-encoding", "UTF-8",
  "-deprecation",
)

lazy val root = (project in file("."))
  .settings(
    name := "dynamodecs-root",
  )
  .aggregate(dynamodecs, integration)

lazy val dynamodecs = (project in file("dynamodecs"))
  .settings(
    name := "dynamodecs",
    libraryDependencies ++= Seq(
      scalatest % Test,
      dynamoDb,
      circeGeneric % Test,
    ) ++ circe,
  )

lazy val integration = (project in file("integration-tests"))
  .settings(
    name := "dynamodecs-integration-tests",
    libraryDependencies ++= Seq(
      scalatest % Test,
      dynamoDb,
      circeGeneric,
      logback,
    ) ++ circe,
    // TODO: replace sbt-dynamodb with a maintained solution
    // update the local dynamoDB instance if it's older than 14 days
    dynamoDBLocalDownloadIfOlderThan := 14.days,
    // this port needs to match the DB setup in the integration tests
    dynamoDBLocalPort := 8042,
    // start DB around tests
    Test / startDynamoDBLocal := (Test / startDynamoDBLocal).dependsOn(Test / compile).value,
    Test / test := (Test / test).dependsOn(Test / startDynamoDBLocal).value,
    Test / testOnly := (Test / testOnly).dependsOn(Test / startDynamoDBLocal).evaluated,
    Test / testOptions += (Test / dynamoDBLocalTestCleanup).value,
  )
  .dependsOn(dynamodecs)
