name := "Graph Visualizer"

version := "1.0.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation")

resourceDirectory in Compile := (scalaSource in Compile).value

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "8.0.60-R9",
  "org.scalafx" %% "scalafxml-core-sfx8" % "0.2.2"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

fork := true
