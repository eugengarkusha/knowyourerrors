name := "knowyourerrors"

version := "0.1"
scalaVersion := "2.12.4"
organization := "com.dbrsn"


libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.7"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

homepage := Some(url("https://github.com/eugengarkusha/knowyourerrors"))

scmInfo := Some(ScmInfo(url("https://github.com/eugengarkusha/knowyourerrors"), "scm:git:git://github.com:eugengarkusha/knowyourerrors.git"))

pomExtra :=
  <developers>
    <developer>
      <id>Ievgen Garkusha</id>
      <name>Denis Borisenko</name>
      <url>https://github.com/eugengarkusha</url>
    </developer>
  </developers>