name := "scex"

version in Global := "1.16.6"
scalaVersion in Global := "2.11.7"
organization in Global := "com.avsystem"
crossPaths in Global := false
scalacOptions in Global ++= Seq(
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

val silencerVersion = "0.3"
val guavaVersion = "18.0"
val jettyVersion = "9.1.0.v20131115"
val vaadinVersion = "6.8.13"
val findbugsVersion = "2.0.1"
val slf4jVersion = "1.6.4"
val logbackVersion = "1.0.6"
val commonsCodecVersion = "1.7"
val junitVersion = "4.11"
val scalatestVersion = "2.1.3"

libraryDependencies in Global ++= Seq(
  compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion),
  "com.github.ghik" % "silencer-lib" % silencerVersion
)

lazy val scex = project.in(file("."))
  .aggregate(core, derived, test, javaTest)
  .settings(
    // for POM generation
    libraryDependencies := (libraryDependencies in core).value
  )
  .settings((
    for (packageTask <- Seq(packageBin, packageSrc)) yield mappings in(Compile, packageTask) :=
      (mappings in(core, Compile, packageTask)).value ++ (mappings in(derived, Compile, packageTask)).value
    ): _*)

lazy val subprojectSettings = Seq(
  publishArtifact := false,
  fork in Test := true,
  outputStrategy in Test := Some(LoggedOutput(new Logger {
    def log(level: Level.Value, message: => String): Unit = ()
    def success(message: => String): Unit = ()
    def trace(t: => Throwable): Unit = ()
  })),
  libraryDependencies ++= Seq(
    "junit" % "junit" % junitVersion % Test,
    "org.scalatest" %% "scalatest" % scalatestVersion % Test
  )
)

lazy val core = project.in(file("scex-core"))
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
      "commons-codec" % "commons-codec" % commonsCodecVersion
    )
  )

lazy val derived = project.in(file("scex-derived")).dependsOn(core)
  .settings(subprojectSettings: _*)

lazy val test = project.in(file("scex-test")).dependsOn(derived)
  .settings(subprojectSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.vaadin" % "vaadin" % vaadinVersion,
      "org.eclipse.jetty" % "jetty-server" % jettyVersion,
      "org.eclipse.jetty" % "jetty-servlet" % jettyVersion
    )
  )

lazy val javaTest = project.in(file("scex-java-test")).dependsOn(derived)
  .settings(subprojectSettings: _*)
  .settings(
    compileOrder := CompileOrder.JavaThenScala
  )
