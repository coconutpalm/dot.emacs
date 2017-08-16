resolvers += "Sonatype releases" at
"https://oss.sonatype.org/content/repositories/releases"

// addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC8")
addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.14")


// allows creating dependency graphs
//
// - https://github.com/jrudolph/sbt-dependency-graph
// - https://github.com/gilt/sbt-dependency-graph-sugar
//
// @see dependencySvg and dependencySvgView

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
addSbtPlugin("com.gilt" % "sbt-dependency-graph-sugar" % "0.8.2")


// nicer prompt - https://github.com/agemooij/sbt-prompt
//addSbtPlugin("com.scalapenos" % "sbt-prompt" % "0.2.1")
