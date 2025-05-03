val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "reps",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "2.0.0",
    libraryDependencies += "com.typesafe" % "config" % "1.4.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )