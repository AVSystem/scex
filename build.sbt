name := "scex"

inThisBuild(Seq(
  organization := "com.avsystem.scex",
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.10"),
))

val CompileAndTest = "compile->compile;test->test"

val parserCombinatorsVersion = "1.1.2"
val collectionCompatVersion = "2.1.2"
val silencerVersion = "1.4.4"
def avsCommonsVersion: Def.Initialize[String] =
  Def.setting(if (scalaBinaryVersion.value == "2.13") "2.0.0-M3" else "1.41.0")
val jettyVersion = "9.1.0.v20131115"
val vaadinVersion = "6.8.13"
val slf4jVersion = "1.6.4"
val logbackVersion = "1.0.6"
val commonsLang3Version = "3.4"
val commonsCodecVersion = "1.7"
val guavaVersion = "23.0"
val commonsNetVersion = "3.3"
val jodaTimeVersion = "2.8.2"
val junitVersion = "4.11"
val scalatestVersion = "3.0.8"

val noPublishSettings = Seq(
  skip in publish := true
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

  fork in Test := true,
  javaOptions in Test += "-Xmx1G",
  outputStrategy in Test := Some(LoggedOutput(new Logger {
    def log(level: Level.Value, message: => String): Unit = ()
    def success(message: => String): Unit = ()
    def trace(t: => Throwable): Unit = ()
  })),
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
    compilerPlugin("com.avsystem.commons" %% "commons-analyzer" % avsCommonsVersion.value),
    "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
    "junit" % "junit" % junitVersion % Test,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  ),
  (sources in doc in Compile) := Seq.empty
)

lazy val scex = project.in(file("."))
  .aggregate(`scex-macros`, `scex-core`, `scex-util`, `scex-test`, `scex-java-test`)
  .settings(noPublishSettings: _*)

lazy val `scex-macros` = project
  .settings(subprojectSettings: _*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies += "com.avsystem.commons" %% "commons-macros" % avsCommonsVersion.value
  )

lazy val `scex-core` = project.dependsOn(`scex-macros` % CompileAndTest)
  .settings(subprojectSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion,
      "org.scala-lang.modules" %% "scala-collection-compat" % collectionCompatVersion,
      "com.avsystem.commons" %% "commons-core" % avsCommonsVersion.value,
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "ch.qos.logback" % "logback-core" % logbackVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "commons-codec" % "commons-codec" % commonsCodecVersion,
      "com.google.guava" % "guava" % guavaVersion
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
      "org.eclipse.jetty" % "jetty-servlet" % jettyVersion
    )
  )

lazy val `scex-java-test` = project.dependsOn(`scex-core`, `scex-util`)
  .settings(subprojectSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    compileOrder := CompileOrder.JavaThenScala
  )
