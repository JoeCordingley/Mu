name := "Mu"

version := "1.0"

scalaVersion := "2.12.2"

val monocleVersion = "1.4.0"

libraryDependencies ++= Seq( "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-law" % monocleVersion % "test",
  "org.typelevel" %% "cats" % "0.9.0",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)