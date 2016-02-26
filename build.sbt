import com.typesafe.sbt.SbtPgp.autoImportImpl.PgpKeys._

name := "scex"

inThisBuild(Seq(
  scalaVersion := "2.11.7",
  organization := "com.avsystem.scex",
  crossPaths := false,
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
    "-Xlint:_,-missing-interpolator,-adapted-args"
  )
))

val CompileAndTest = "compile->compile;test->test"

val silencerVersion = "0.3"
val avsCommonsVersion = "1.13.0"
val guavaVersion = "18.0"
val jettyVersion = "9.1.0.v20131115"
val vaadinVersion = "6.8.13"
val findbugsVersion = "2.0.1"
val slf4jVersion = "1.6.4"
val logbackVersion = "1.0.6"
val commonsCodecVersion = "1.7"
val junitVersion = "4.11"
val scalatestVersion = "2.1.3"

val noPublishSettings = Seq(
  publishArtifact := false,
  publish := {},
  publishLocal := {},
  publishM2 := {},
  publishSigned := {},
  publishLocalSigned := {}
)

lazy val subprojectSettings = Seq(
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
    licenses = Seq(
      ("The MIT License", url("https://opensource.org/licenses/MIT"))
    )
  ),

  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  pomExtra := {
    <developers>
      <developer>
        <id>ghik</id>
        <name>Roman Janusz</name>
        <url>https://github.com/ghik</url>
      </developer>
    </developers>
  },

  fork in Test := true,
  javaOptions in Test += "-Xmx1G",
  outputStrategy in Test := Some(LoggedOutput(new Logger {
    def log(level: Level.Value, message: => String): Unit = ()
    def success(message: => String): Unit = ()
    def trace(t: => Throwable): Unit = ()
  })),
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion),
    compilerPlugin("com.avsystem.commons" %% "commons-analyzer" % avsCommonsVersion),
    "com.github.ghik" % "silencer-lib" % silencerVersion,
    "junit" % "junit" % junitVersion % Test,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  )
)

lazy val scex = project.in(file("."))
  .aggregate(`scex-macros`, `scex-core`, `scex-test`, `scex-java-test`)
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
      "com.google.guava" % "guava" % guavaVersion,
      "com.google.code.findbugs" % "jsr305" % findbugsVersion,
      "org.slf4j" % "slf4j-api" % slf4jVersion,
      "ch.qos.logback" % "logback-core" % logbackVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "commons-codec" % "commons-codec" % commonsCodecVersion,
      "com.avsystem.commons" %% "commons-core" % avsCommonsVersion
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
