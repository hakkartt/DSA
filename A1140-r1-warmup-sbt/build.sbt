lazy val root = (project in file(".")).
  settings(
    name := "A1140-r1-warmup-sbt",
    version := "1.0",
    scalaVersion := "2.12.2",
    parallelExecution in Test := false,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3"

  )
