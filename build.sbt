
name := "knowyourerrors"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.4"
organization := "com.dbrsn"

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
      <name>Ievgen Garkusha</name>
      <url>https://github.com/eugengarkusha</url>
    </developer>
  </developers>


lazy val root = project.in(file(".")).
  aggregate(projectJS, projectJVM).
  settings(
    publish := {},
    publishLocal := {},
    libraryDependencies ++= "com.chuusai" %%% "shapeless" % "2.3.3" ::
      "org.typelevel" %%% "cats-core" % "1.1.0" ::
      "org.scalatest" %%% "scalatest" % "3.0.5" % Test :: Nil
  )


lazy val proj = _root_.sbtcrossproject.CrossPlugin.autoImport.crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings().
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
// Add JS-specific settings here
  )

lazy val projectJVM = proj.jvm
lazy val projectJS = proj.js