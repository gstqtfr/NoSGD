import sbt.Keys._

name := "FBackProp"

version := "3.7.1"

scalaVersion := "2.11.8"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// additional libraries
libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core"                     % "2.1.0" % "provided",
  "org.apache.spark" %% "spark-sql"                      % "2.1.0",
  "org.apache.spark" %% "spark-mllib"                    % "2.1.0",
  "org.apache.spark" %% "spark-streaming"                % "2.1.0",
  "org.json4s"       %% "json4s-jackson"                 % "3.2.11",
  "org.json4s"       %% "json4s-ext"                     % "3.4.0",
  "joda-time"         % "joda-time"                      % "2.9.7",
  "com.github.nscala-time" % "nscala-time_2.11"          % "2.16.0",
  "log4j"             % "log4j"                          % "1.2.17",
  "org.scalanlp"      % "breeze_2.11"                    % "0.13",
  "org.scalactic"    %% "scalactic"                      % "3.0.1",
  "org.scalatest"    %% "scalatest"                      % "3.0.1" % "test",
  "log4j"             % "log4j"                          % "1.2.17",
  "org.scalanlp"     %% "breeze"                         % "0.9",
  "org.scalanlp"     %% "breeze-config"                  % "0.9.1",
  "org.scalanlp"     %% "breeze-natives"                 % "0.8" % "test, runtime"
)

test in assembly := {}

//scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq(
  "JBoss Repository" at "http://repository.jboss.org/nexus/content/repositories/releases/",
  "Spray Repository" at "http://repo.spray.cc/",
  "Cloudera Repository" at "https://repository.cloudera.com/artifactory/cloudera-repos/",
  "Akka Repository" at "http://repo.akka.io/releases/",
  "Twitter4J Repository" at "http://twitter4j.org/maven2/",
  "Apache HBase" at "https://repository.apache.org/content/repositories/releases",
  "Twitter Maven Repo" at "http://maven.twttr.com/",
  "scala-tools" at "https://oss.sonatype.org/content/groups/scala-tools",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "Second Typesafe repo" at "http://repo.typesafe.com/typesafe/maven-releases/",
  "Mesosphere Public Repository" at "http://downloads.mesosphere.io/maven",
  "mth.io snapshots"  at "http://repo.mth.io/snapshots",
  "mth.io releases"  at "http://repo.mth.io/releases",
  "Artima Maven Repository" at "http://repo.artima.com/releases",
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases",
  Resolver.sonatypeRepo("public")
)

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
{
  case m if m.toLowerCase.endsWith("manifest.mf") => MergeStrategy.discard
  case m if m.startsWith("META-INF") => MergeStrategy.discard
  case PathList("javax", "servlet", xs @ _*) => MergeStrategy.first
  case PathList("org", "apache", xs @ _*) => MergeStrategy.first
  case PathList("org", "jboss", xs @ _*) => MergeStrategy.first
  case "about.html"  => MergeStrategy.rename
  case "reference.conf" => MergeStrategy.concat
  case _ => MergeStrategy.first
}
}

