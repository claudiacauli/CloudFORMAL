name := "CloudLogic"

version := "0.1"

scalaVersion := "2.13.0"
mainClass in (Compile,run) := Some("src.main.scala.Main")
updateOptions := updateOptions.value.withCachedResolution(true)
licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.3"
libraryDependencies += "org.apache.commons" % "commons-configuration2" % "2.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "junit" % "junit" % "4.10" % Test
libraryDependencies += "org.slf4j" % "slf4j-jdk14" % "1.7.26"
libraryDependencies += "net.sourceforge.owlapi" % "owlapi-api" % "5.1.11"
libraryDependencies += "net.sourceforge.owlapi" % "owlapi-apibinding" % "5.1.11"
libraryDependencies += "org.apache.commons" % "commons-rdf-api" % "0.5.0"