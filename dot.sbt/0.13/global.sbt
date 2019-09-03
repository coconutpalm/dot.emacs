import org.ensime.EnsimeKeys._

// The latest Ensime stable build + settings
ensimeServerVersion in ThisBuild := "2.0.0"
ensimeProjectServerVersion in ThisBuild := "2.0.0"
ensimeIgnoreMissingDirectories := true
ensimeJavaFlags ++= Seq("-Xmx5g")
ensimeServerFindUsages in ThisBuild := true
transitiveClassifiers := Seq("sources")

// Kill subprocesses if interrupted with Ctrl-C
cancelable in Global := true

// setup for sbt-dependency-graph
//net.virtualvoid.sbt.graph.Plugin.graphSettings


libraryDependencies ++= Seq("com.lihaoyi" %% "pprint" % "0.5.3")