load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell._
val beetl=root/'Users/'dorme/'shopsmart/"beetl-deps"/'beetl
val pwd=wd
ammonite.shell.Configure(repl, wd)
