val commonSettings = Seq(
  scalaVersion := "2.13.2",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  logBuffered in Test := false
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
