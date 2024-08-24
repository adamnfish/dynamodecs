import sbt.*


object Dependencies {
  object Versions {
    val scalatest = "3.2.18"

    val dynamoDb = "2.25.35"

    val circe = "0.14.7"

    val logback = "1.5.7"
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
