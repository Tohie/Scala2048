assemblySettings

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "org.specs2" %% "specs2" % "2.3.10" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

