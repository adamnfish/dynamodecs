import sbt.*


object Dependencies {
  object Versions {
    val scalatest = "3.2.17"

    val dynamoDb = "2.20.68"

    val circe = "0.14.5"

    val logback = "1.4.0"
  }

  val dynamoDb = "software.amazon.awssdk" % "dynamodb" % Versions.dynamoDb
  val scalatest = "org.scalatest" %% "scalatest" % Versions.scalatest
  val circe = Seq(
    "io.circe" %% "circe-core" % Versions.circe,
    "io.circe" %% "circe-parser" % Versions.circe,
  )
  val circeGeneric = "io.circe" %% "circe-generic" % Versions.circe
  val logback = "ch.qos.logback" % "logback-classic" % Versions.logback
}
