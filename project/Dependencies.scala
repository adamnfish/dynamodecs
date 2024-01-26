import sbt.*


object Dependencies {
  object Versions {
    val scalatest = "3.2.17"

    val dynamoDb = "2.20.68"

    val circe = "0.14.5"
  }

  lazy val dynamoDb = "software.amazon.awssdk" % "dynamodb" % Versions.dynamoDb
  lazy val scalatest = "org.scalatest" %% "scalatest" % Versions.scalatest
  lazy val circe = Seq(
    "io.circe" %% "circe-core" % Versions.circe,
    "io.circe" %% "circe-parser" % Versions.circe,
  )
  lazy val circeGeneric = "io.circe" %% "circe-generic" % Versions.circe
}
