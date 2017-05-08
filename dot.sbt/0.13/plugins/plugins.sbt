scalaVersion := "2.10.6"

resolvers +=
"Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC1")
addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.10")
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.1.0")
