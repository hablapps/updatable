name := "UPDATABLE"

projectVersion in ThisBuild := ("0.7.1",BRANCH of "NONWEAK")

organization in ThisBuild := "org.hablapps"

scalaVersion in ThisBuild := "2.10.2"

scalaBinaryVersion in ThisBuild  <<= scalaVersion {(sv: String) => sv}

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

libraryDependencies in ThisBuild <++= scalaVersion { (sv: String) => Seq(
	"org.scala-lang" % "scala-compiler" % sv,
	"org.scala-lang" % "scala-reflect" % sv,
	"org.scala-lang" % "scala-actors" % "2.10.2-RC1",
	"org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
	"junit" % "junit" % "4.10"
)}

publishMavenStyle := true

publishArtifact in (Compile, packageSrc) := false

publish ~= { (publish) =>
  publish
  publishExtra
}

version in ThisBuild <<= projectVersion(pv => pv match{
  case (version,publishType) => publishType match{
    case RELEASE => version
    case SNAPSHOT => version + revision + "-" + { "svnversion" !! match { case s => s.take(s.length-1).replace(":","-") } }
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
