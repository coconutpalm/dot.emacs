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

def c(dir : Path) = { cd! dir }

def l = { ls! }
def predefs = { %vim home/".emacs.d"/"predef.scala" }

def make(targets : String*) = {
   def multiMake(targets : String*) : Unit = targets.toList match {
      case target :: Nil => %make(target)
      case target :: more => %make(target); multiMake(more : _*)
   }
   if (targets.length == 0) {
      %make()
   } else {
      multiMake(targets : _*)
   } 
}

ammonite.shell.Configure(repl, wd)
