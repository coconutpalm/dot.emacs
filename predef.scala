load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell._

implicit def symbol2str(s : Symbol) : String = s.name
implicit def path2str(p : Path) : String = {
  p.segments.foldLeft("") { (cur,next) => cur + java.io.File.separator + next }
}
implicit def relpath2str(p : RelPath) : String = {
  val absPath = wd/p
  absPath.segments.foldLeft("") { (cur,next) => cur + java.io.File.separator + next }
}

val beetl=root/'Users/'dorme/'shopsmart/"beetl-deps"/'beetl
val dim=root/'Users/'dorme/'dev/'datainmotion/"datainmotion-server"

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
def branches = { %git 'branch }
def pull = { %git 'pull }
def push = { %git 'push }
def merge(branch: String) = { %git('merge, branch) }
def commit = { %git('commit) }
def commitAll = { %git('commit, "-a") }
def commitAll(message: String) = { %git('commit, "-a", "-m", message)}

def bash = { %bash("-l") }

def c(dir : String) = { cd! dir }

def l = { ls!  }
def make = { %make }
def predefs = { %vim home/".emacs.d"/"predef.scala" }

ammonite.shell.Configure(repl, wd)
