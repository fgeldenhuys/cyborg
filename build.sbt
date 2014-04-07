// Name of your app
name := "cyborg"

// Version of your app
version := "0.1"

// Version of Scala
scalaVersion := "2.10.3"

libraryDependencies := Seq(
  "com.google.android" % "android" % "4.0.1.2",
  "com.google.android" % "support-v4" % "r7",
  "org.scala-lang" % "scala-library" % "2.10.3",
  "org.scalaz" %% "scalaz-core" % "7.0.4",
  "org.scalaz" %% "scalaz-concurrent" % "7.0.4"
)
