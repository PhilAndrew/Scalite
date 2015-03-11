val sharedSettings = Seq(
  organization  := "com.lihaoyi",
  version := "0.1.0",
  pomExtra :=
    <url>https://github.com/PhilAndrew/Scalite</url>
      <licenses>
        <license>
          <name>MIT license</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/PhilAndrew/Scalite.git</url>
        <connection>scm:git://github.com/PhilAndrew/Scalite.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
)
lazy val api = project.settings(sharedSettings:_*).settings(
  name := "scalite-phil",
  version       := scalite.SbtPlugin.scaliteVersion,
  scalaVersion  := "2.11.4",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  libraryDependencies += "com.moandjiezana.toml" % "toml4j" % "0.4.0",
  (resources in Test) ++=  (managedClasspath in Compile).value.map(_.data)
)

lazy val scaliteSbtPlugin = project.settings(sharedSettings:_*)
  .settings(
    name := "scalite-sbt-plugin-phil",
    scalaVersion := "2.10.4",
    sbtPlugin := true
  )

lazy val example = project.settings(sharedSettings ++ scalite.SbtPlugin.projectSettings:_*)
                              .settings(
  scalaVersion  := "2.11.4",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.2.4",
  testFrameworks += new TestFramework("utest.runner.JvmFramework"),
  (compile in Compile) <<= (compile in Compile).dependsOn(publishLocal in api),
  publishArtifact := false
)

lazy val sprayTemplate = project.settings(scalite.SbtPlugin.projectSettings:_*).settings(
  organization  := "com.example",
  version       := "0.1",
  scalaVersion  := "2.11.2",
  scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8"),
  libraryDependencies ++= {
    val akkaV = "2.3.6"
    val sprayV = "1.3.2"
    Seq(
      "io.spray"            %%  "spray-can"     % sprayV,
      "io.spray"            %%  "spray-routing" % sprayV,
      "io.spray"            %%  "spray-testkit" % sprayV  % "test",
      "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
      "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
      "org.specs2"          %%  "specs2-core"   % "2.3.11" % "test"
    )
  }
)

lazy val scalajsExample = project.settings(scalaJSSettings ++ scalite.SbtPlugin.projectSettings:_*)
                                      .settings(
  name := "Example",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.2",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules.scalajs" %%% "scalajs-dom" % "0.6"
  )
)

publishArtifact := false


