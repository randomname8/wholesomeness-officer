lazy val `wholesomeness-officer` = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "wholesomeness-officer",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "wholesomeness-officer",

    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.5.12",
      "io.github.soc" %% "regextractor" % "0.2",
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-typed" % akkaVersion,
      "com.github.austinv11" % "Discord4J" % "2.9"
    ),

    resolvers += "jitpack.io" at "https://jitpack.io"
  )

lazy val akkaVersion = "2.5.6"