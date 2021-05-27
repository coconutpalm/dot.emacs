(ns server.SWT)

(def platform ->> (System/getProperty "os.name"))

(def platform-libs
  {:linux   ['org.eclipse.swt/org.eclipse.swt.gtk.linux.x86_64    "4.3"]
   :mac     ['org.eclipse.swt/org.eclipse.swt.cocoa.macosx.x86_64 "4.3"]
   :windows ['org.eclipse.swt/org.eclipse.swt.win32.win32.x86_64  "4.3"]})

  val linux = "org.eclipse.swt" % "org.eclipse.swt.gtk.linux.x86_64" % "4.3"
  val mac = "org.eclipse.swt" % "org.eclipse.swt.cocoa.macosx.x86_64" % "4.3"
  val wind = "org.eclipse.swt" % "org.eclipse.swt.win32.win32.x86_64" % "4.3"


(def library nil)
