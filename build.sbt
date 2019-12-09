
name := "magsquare"

version := "0.1.0"

organization := "arrufat.org"

scalaVersion := "2.13.1"

scalacOptions ++= Seq("–unchecked", "-deprecation")


resolvers += Resolver.sonatypeRepo("releases")



libraryDependencies ++= {
  	Seq(
			"org.scalactic" %% "scalactic" % "3.1.0",
			"org.scalatest" %% "scalatest" % "3.1.0" % "test",
	    "org.scala-graph" %% "graph-core" % "1.13.1"
  	)
}









