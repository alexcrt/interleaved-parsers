name := "parser-combinators"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

//Scala Meter
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false