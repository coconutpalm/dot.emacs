import org.ensime.EnsimeCoursierKeys._
import org.ensime.EnsimeKeys._

// The latest Ensime stable build + settings
ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
ensimeProjectServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
ensimeIgnoreMissingDirectories := true

// Kill subprocesses if interrupted with Ctrl-C
cancelable in Global := true
