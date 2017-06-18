resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Spray Repository" at "http://repo.spray.cc/"

//compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.1")

//addSbtPlugin("com.github.gseitz" % "sbt-protobuf" % "0.3.3")
