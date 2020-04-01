
resolvers += "Sonatype releases" at
"https://oss.sonatype.org/content/repositories/releases"

//addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.0-RC1-76-1488031d")
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.0-RC1-69-693de22a")

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
