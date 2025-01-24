name := "fpinscala"

ThisBuild / scalaVersion := "3.3.4"

ThisBuild / githubWorkflowBuild := Seq(WorkflowStep.Sbt(name = Some("Build project"), commands = List("test:compile")))

ThisBuild / scalacOptions ++= List("-feature", "-deprecation", "-Ykind-projector:underscores", "-source:future")

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

// https://mvnrepository.com/artifact/org.typelevel/cats-effect
ThisBuild / libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6-0142603"
