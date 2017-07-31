//scalaVersion := "2.10.6"

resolvers +=
"Sonatype releases" at "https://oss.sonatype.org/content/repositories/releases"

//addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC1")
//addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.10")
//addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.1.0")

// allows creating dependency graphs - https://github.com/jrudolph/sbt-dependency-graph
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.5")

// nicer prompt - https://github.com/agemooij/sbt-prompt
//addSbtPlugin("com.scalapenos" % "sbt-prompt" % "0.2.1")
