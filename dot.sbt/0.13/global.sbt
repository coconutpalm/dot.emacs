
// Kill subprocesses if interrupted with Ctrl-C
cancelable in Global := true

// setup for sbt-dependency-graph
//net.virtualvoid.sbt.graph.Plugin.graphSettings


val DebugTest = config("debug") extend Test
def addDebugTest(p: Project) =
  p.configs(DebugTest).
  settings(inConfig(DebugTest)(Defaults.testSettings):_*).
  settings(
    fork in DebugTest := true,
    javaOptions in DebugTest += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=127.0.0.1:9999",
    definedTests in DebugTest := (definedTests in Test).value
  )


libraryDependencies ++= Seq("com.lihaoyi" %% "pprint" % "0.5.3")
