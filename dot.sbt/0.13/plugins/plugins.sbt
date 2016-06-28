scalaVersion := "2.10.6"

resolvers +=
"Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M12")

addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.4")
