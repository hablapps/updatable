name := "UPDATABLE"

projectVersion in ThisBuild := ("0.7.2", RELEASE)

organization in ThisBuild := "org.hablapps"

scalaVersion in ThisBuild := "2.11.12"

scalacOptions ++= Seq("-feature", "-deprecation", "-language:reflectiveCalls", "-language:experimental.macros")

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)

libraryDependencies in ThisBuild <++= scalaVersion { (sv: String) => Seq(
	"org.scala-lang" % "scala-compiler" % sv,
	"org.scala-lang" % "scala-reflect" % sv,
	"org.scalatest" %% "scalatest" % "3.0.0" % "test",
	"junit" % "junit" % "4.10" % "test",
  "org.scalaz" %% "scalaz-core" % "7.0.6"
)}

parallelExecution in Test := false

publishMavenStyle := true

incOptions := incOptions.value.withNameHashing(true)

publishArtifact in (Compile, packageSrc) := false

publish ~= { (publish) =>
  publish
  // publishExtra
}

version in ThisBuild <<= projectVersion(pv => pv match{
  case (version,publishType) => publishType match{
    case RELEASE => version
    case SNAPSHOT => version + revision + "-" + { "git rev-parse HEAD" !! match { case s => s.take(s.length-1) } }
    case BRANCH(name) => version + "-" + name
  }
})

publishTo <<= projectVersion { pv =>
  pv match{
    case (version,publishType) => {
      val repo_loc = publishType match{
        case RELEASE => "/var/www/repo/releases"
        case BRANCH(_) => "/var/www/private-repo/snapshots"
        case SNAPSHOT => "/var/www/repo/snapshots"
      }
      Some(Resolver.sftp("Speech repository", "andromeda", repo_loc))
    }
  }
}
