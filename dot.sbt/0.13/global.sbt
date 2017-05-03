import org.ensime.EnsimeKeys._

// The latest Ensime stable build
ensimeServerVersion in ThisBuild := "1.12.10"
ensimeProjectServerVersion in ThisBuild := "1.12.10"

// Kill subprocesses if interrupted with Ctrl-C
cancelable in Global := true
