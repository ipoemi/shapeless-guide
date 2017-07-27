val macroParadise = compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
val kindProjector = compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
val shapeless = "com.chuusai" %% "shapeless" % "2.3.2"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "ipoemi",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "shapeless-guide",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-language:_"
    ),
    libraryDependencies ++= Seq(
      shapeless, macroParadise, kindProjector,
      scalaCheck % Test, scalaTest % Test
    )
  )
