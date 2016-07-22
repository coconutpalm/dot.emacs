//Slick is at: http://slick.lightbend.com/doc/3.1.1/code-generation.html

load.ivy("com.typesafe.slick" %% "slick" % "3.1.1")
load.ivy("com.typesafe.slick" %% "slick-codegen" % "3.1.1")
load.ivy("org.slf4j" % "slf4j-nop" % "1.6.4")
load.ivy("com.lihaoyi" %% "scalarx" % "0.3.1")

load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)

@
val shellSession = ammonite.shell.ShellSession()

import shellSession._
import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell._
import rx._

import slick.driver.PostgresDriver.api._
import scala.concurrent.ExecutionContext.Implicits.global


val redshift=System.getProperty("REDSHIFT")
val redshift_user=System.getProperty("RS_USER")
val redshift_pass=System.getProperty("RS_PASS")
val redshift_db=System.getProperty("RS_DB")
val redshift_port=System.getProperty("RS_PORT")


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
def emacsd = { %vim home/".emacs.d"/"emacs-init.el" }

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

// @see http://www.webjars.org/documentation
def webjar(lib : String, asset : String, version : String) = s"//cdn.jsdelivr.net/webjars/org.webjars/${lib}/${version}/${asset}"


ammonite.shell.Configure(repl, wd)

