import com.typesafe.sbt.SbtPgp.autoImportImpl.PgpKeys._

name := "scex"

inThisBuild(Seq(
  organization := "com.avsystem.scex",
  scalaVersion := "2.12.7",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.12"),
))

val CompileAndTest = "compile->compile;test->test"

val parserCombinatorsVersion = "1.0.5"
val silencerVersion = "1.2.1"
val avsCommonsVersion = "1.34.0"
val jettyVersion = "9.1.0.v20131115"
val vaadinVersion = "6.8.13"
val slf4jVersion = "1.6.4"
val logbackVersion = "1.0.6"
val commonsLang3Version = "3.4"
val commonsCodecVersion = "1.7"
val commonsNetVersion = "3.3"
val jodaTimeVersion = "2.8.2"
val junitVersion = "4.11"
val scalatestVersion = "3.0.0"
val guavaVersion = "23.0"

val noPublishSettings = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {},
  publishM2 := {},
  publishSigned := {},
  publishLocalSigned := {}
)

sonatypeProfileName := "com.avsystem"

lazy val subprojectSettings = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics",
    "-language:experimental.macros",
    "-Xfuture",
    "-Xfatal-warnings",
    s"-Xlint:-missing-interpolator,-adapted-args,${if (scalaBinaryVersion.value == "2.12") "-unused," else ""}_"
  ),

  sonatypeProfileName := "com.avsystem",
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },

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
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
    compilerPlugin("com.avsystem.commons" %% "commons-analyzer" % avsCommonsVersion),
    "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided,
    "junit" % "junit" % junitVersion % Test,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  )
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

lazy val `scex-test` = project.dependsOn(`scex-core`)
  .settings(subprojectSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.vaadin" % "vaadin" % vaadinVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "org.eclipse.jetty" % "jetty-servlet" % jettyVersion
    )
  )

lazy val `scex-java-test` = project.dependsOn(`scex-core`)
  .settings(subprojectSettings: _*)
  .settings(noPublishSettings: _*)
  .settings(
    compileOrder := CompileOrder.JavaThenScala
  )
