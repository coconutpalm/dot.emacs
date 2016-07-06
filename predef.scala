load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell._

val beetl=root/'Users/'dorme/'shopsmart/"beetl-deps"/'beetl
val dill="djo@dill.local"
val sage="dorme@sage.local"

def pwd=wd

def ssh(host : String) = { %ssh("-X", host) }
def emacs(file : String) = { %emacs(file) }
def vim(file : String) = { %vim(file) }

def status = { %git('status) }
def checkout(branch : String) = { %git('checkout, branch) }
def gitclone(url : String) = { %git('clone, "--recursive", url) }
def submodule(url : String) = { %git('submodule, 'add, url) }
def update = { %git('submodule, 'update, "--init") }
def branchTo(newBranch : String) = { %git('checkout, "-b", newBranch) }
def branches = { %git 'branch}
def pull = { %git 'pull}
def merge(branch: String) = { %git('merge, branch) }
def commit = { %git('commit) }
def commitall = { %git('commit, "-a") }
def commitall(message: String) = { %git('commit, "-a", "-m", message)}

def bash = { %bash("-l") }

def c(dir : Symbol) = { cd! dir }
def c(dir : String) = { cd! dir }
def c(dir : RelPath) = { cd! dir }
def c(dir : Path) = { cd! dir }

def l = { ls!  }

ammonite.shell.Configure(repl, wd)
