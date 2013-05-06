import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "cml"
  val buildVersion = "0.1.0-SNAPSHOT"
  val buildScalaVersion = "2.10.1"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    exportJars := true,
    scalacOptions ++= Seq ("-deprecation", "-feature", "-language:postfixOps",
      "-language:higherKinds"),
    initialCommands in console := """
      import scalaz._, Scalaz._
      import cml._, cml.xml._, cml.xml.Xml._
      import scalaz.xml.Xml._
      val atom = Atom("a1", Element.Br, Some(-1))
    """
  )

} 

object Dependencies {
  val scalaz = "org.scalaz"
  val scalazV = "7.0.0"

  val scalaz_core = scalaz %% "scalaz-core" % scalazV
  val scalaz_xml = scalaz %% "scalaz-xml" % scalazV
  val scalaz_effect = scalaz %% "scalaz-effect" % scalazV
  val scalaz_iteratee = scalaz %% "scalaz-iteratee" % scalazV
  val scalaz_scalacheck = scalaz %% "scalaz-scalacheck-binding" % scalazV % "test"

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"
}

object UtilBuild extends Build {
  import Dependencies._
  import BuildSettings._

  def addDeps (ds: ModuleID*) =
    BuildSettings.buildSettings ++
    Seq (libraryDependencies ++= ds) ++
    com.github.retronym.SbtOneJar.oneJarSettings

  lazy val cml = Project (
    "cml",
    file("."),
    settings = addDeps (scalaz_core, scalaz_effect, scalaz_iteratee,
                        scalaz_scalacheck, scalacheck, scalaz_xml)
  )
}

// vim: set ts=2 sw=2 et:
