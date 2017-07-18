name := "Fuck-Gradient-Descent"

version := "3.7.1"

scalaVersion := "2.11.7"

scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-target:jvm-1.8", "-deprecation", "-feature", "-unchecked", "-Xlog-reflective-calls", "-Xlint")

libraryDependencies ++= Seq(
  "org.jblas"         % "jblas"                          % "1.2.2",
  "com.typesafe.akka" % "akka-actor_2.11"                % "2.5.3",
  "org.scalatest"    %% "scalatest"                      % "3.0.1" % "test",
  "junit"             % "junit"                          % "4.12" % Test,
  "com.novocode"      % "junit-interface"                % "0.11" % Test
)

test in assembly := {}
