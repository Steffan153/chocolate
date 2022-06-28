val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Chocolate",
    version := "0.0.1",
    organization := "chocolate",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      ("org.typelevel" %% "spire" % "0.17.0").cross(CrossVersion.for3Use2_13)
    ),
    scalacOptions ++= Seq(
      "-encoding",
      "utf8",
      "-deprecation"
    )
  )
