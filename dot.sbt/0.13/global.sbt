import org.ensime.EnsimeKeys._
import org.ensime.EnsimeCoursierKeys._

// The latest Ensime stable build + settings
ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
ensimeProjectServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
ensimeIgnoreMissingDirectories := true

// Kill subprocesses if interrupted with Ctrl-C
cancelable in Global := true

// Use Ammonite for the Scala repl
libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.8.0" % "test" cross CrossVersion.full

//initialCommands in (Test, console) := """ammonite.Main().run()"""
