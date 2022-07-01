val scala3Version = "3.1.3"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .aggregate(chocolate.js, chocolate.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val chocolate = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "Chocolate",
    version := "0.0.1",
    organization := "chocolate",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      ("org.typelevel" %%% "spire" % "0.17.0").cross(CrossVersion.for3Use2_13),
      ("io.circe" %%% "circe-core" % "0.14.1").cross(CrossVersion.for3Use2_13),
      ("io.circe" %%% "circe-generic" % "0.14.1").cross(CrossVersion.for3Use2_13),
      ("io.circe" %%% "circe-parser" % "0.14.1").cross(CrossVersion.for3Use2_13)
    ),
    scalacOptions ++= Seq(
      "-encoding",
      "utf8",
      "-deprecation"
    ),
  )
  .jvmSettings(
    Compile / mainClass := Some("chocolate.JVMMain"),
    assembly / assemblyJarName := "Chocolate.jar",
    assembly / mainClass := Some("chocolate.JVMMain"),
  )
  .jsSettings(
    Compile / fastOptJS / artifactPath := baseDirectory.value / "lib" / s"scalajs-0.0.1.js",
    Compile / fullOptJS / artifactPath := baseDirectory.value / "lib" / s"scalajs-0.0.1.js",
  )
