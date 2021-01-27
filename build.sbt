
scalaVersion := "2.13.2"

enablePlugins(SbtOsgi)
OsgiKeys.exportPackage := Seq("coevo.*;-split-package:=merge-first")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!scala.*,META-INF.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
osgiSettings
mainClass in(Compile, run) := Some("coevo.Coevo")
