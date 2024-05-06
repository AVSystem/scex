name := "scex"

inThisBuild(Seq(
  organization := "com.avsystem.scex",
  scalaVersion := "2.13.14",

  githubWorkflowTargetTags ++= Seq("v*"),
  githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11")),
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
val avsCommonsVersion = "2.15.0"
val jettyVersion = "9.4.54.v20240208" // Tests only
val vaadinVersion = "6.8.18" // Tests only
val slf4jVersion = "2.0.13"
val logbackVersion = "1.5.6" // Tests only
val commonsLang3Version = "3.14.0"
val commonsCodecVersion = "1.17.0"
val guavaVersion = "33.2.0-jre"
val commonsNetVersion = "3.10.0"
val jodaTimeVersion = "2.12.7"
val scalatestVersion = "3.2.18"

val noPublishSettings = Seq(
  publish / skip := true
)

sonatypeProfileName := "com.avsystem"

lazy val subprojectSettings = Seq(
  crossVersion := CrossVersion.full,

  javacOptions ++= Seq(
    "-source", "1.8",
    "-target", "1.8",
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

  scalacOptions ++= {
    if (scalaBinaryVersion.value == "2.13") Seq(
      "-Xnon-strict-patmat-analysis",
      "-Xlint:-strict-unsealed-patmat"
    ) else Seq.empty
  },

  sonatypeProfileName := "com.avsystem",
  publishTo := sonatypePublishToBundle.value,

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
    licenses = Vector(
      ("The MIT License", url("https://opensource.org/licenses/MIT"))
    ),
    developers = Vector(
      Developer("ghik", "Roman Janusz", "romeqjanoosh@gmail.com", url("https://github.com/ghik"))
    )
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
  .aggregate(`scex-macros`, `scex-core`, `scex-util`, `scex-test`, `scex-java-test`)
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
      "joda-time" % "joda-time" % jodaTimeVersion
    )
  )

lazy val `scex-test` = project.dependsOn(`scex-core`, `scex-util`)
  .settings(subprojectSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.vaadin" % "vaadin" % vaadinVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "org.eclipse.jetty" % "jetty-servlet" % jettyVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
    )
  )

lazy val `scex-java-test` = project.dependsOn(`scex-core`, `scex-util`)
  .settings(subprojectSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    compileOrder := CompileOrder.JavaThenScala
  )
