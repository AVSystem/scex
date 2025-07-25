name := "scex"

inThisBuild(Seq(
  organization := "com.avsystem.scex",
  scalaVersion := "2.13.16",
  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21")),
  githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v"))),

  githubWorkflowPublish := Seq(WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )),
))

val CompileAndTest = "compile->compile;test->test"

val parserCombinatorsVersion = "2.4.0"
val avsCommonsVersion = "2.23.1"
val slf4jVersion = "2.0.17"
val logbackVersion = "1.5.18" // Tests only
val commonsLang3Version = "3.18.0"
val commonsCodecVersion = "1.19.0"
val guavaVersion = "33.4.8-jre"
val commonsNetVersion = "3.11.1"
val scalatestVersion = "3.2.19"

val noPublishSettings = Seq(
  publish / skip := true
)

lazy val subprojectSettings = Seq(
  crossVersion := CrossVersion.full,

  javacOptions ++= Seq(
    "--release", "17",
    "-parameters"
  ),

  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics",
    "-language:experimental.macros",
    "-Xfatal-warnings",
    "-Xlint:-missing-interpolator,-adapted-args,-unused,_"
  ),

  scalacOptions ++= Seq(
    "-Xnon-strict-patmat-analysis",
    "-Xlint:-strict-unsealed-patmat"
  ),

  projectInfo := ModuleInfo(
    nameFormal = "SCEX",
    description = "Extensible, fast and secure Scala expression evaluation engine",
    homepage = Some(url("https://github.com/AVSystem/scex")),
    startYear = Some(2015),
    organizationName = "AVSystem",
    organizationHomepage = Some(url("http://www.avsystem.com/")),
    scmInfo = Some(ScmInfo(
      browseUrl = url("https://github.com/AVSystem/scex.git"),
      connection = "scm:git:git@github.com:AVSystem/scex.git",
      devConnection = Some("scm:git:git@github.com:AVSystem/scex.git")
    )),
    licenses = Vector(License.MIT),
    developers = Vector(
      Developer("ddworak", "Dawid Dworak", "d.dworak@avsystem.com", url("https://github.com/ddworak")),
    ),
  ),

  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },

  Test / fork := true,
  Test / javaOptions += "-Xmx1G",
  Test / outputStrategy := Some(LoggedOutput(new Logger {
    def log(level: Level.Value, message: => String): Unit = ()
    def success(message: => String): Unit = ()
    def trace(t: => Throwable): Unit = ()
  })),
  libraryDependencies ++= Seq(
    compilerPlugin("com.avsystem.commons" %% "commons-analyzer" % avsCommonsVersion),
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  ),
  Compile / doc / sources := Seq.empty
)

lazy val scex = project.in(file("."))
  .aggregate(`scex-macros`, `scex-core`, `scex-util`, `scex-test`)
  .settings(noPublishSettings: _*)

lazy val `scex-macros` = project
  .settings(subprojectSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.avsystem.commons" %% "commons-macros" % avsCommonsVersion
  )

lazy val `scex-core` = project.dependsOn(`scex-macros` % CompileAndTest)
  .settings(subprojectSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion,
      "com.avsystem.commons" %% "commons-core" % avsCommonsVersion,
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "commons-codec" % "commons-codec" % commonsCodecVersion,
      "com.google.guava" % "guava" % guavaVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion % Test,
    )
  )

lazy val `scex-util` = project.dependsOn(`scex-core` % CompileAndTest)
  .settings(subprojectSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-lang3" % commonsLang3Version,
      "commons-net" % "commons-net" % commonsNetVersion,
    )
  )

lazy val `scex-test` = project.dependsOn(`scex-core` % CompileAndTest, `scex-util`)
  .settings(subprojectSettings: _*)
  .settings(noPublishSettings: _*)