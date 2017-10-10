lazy val `wholesomeness-officer` = (project in file(".")).enablePlugins(JavaAppPackaging).
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
      "org.slf4j" % "slf4j-simple" % "1.7.25",
      "com.github.austinv11" %% "Discord4J" % "2.9.1"
    ),

    resolvers += "jcenter" at "http://jcenter.bintray.com",
    resolvers += "jitpack.io" at "https://jitpack.io",

    bashScriptExtraDefines += """cd "${app_home}/..""""
  )

lazy val akkaVersion = "2.5.6"