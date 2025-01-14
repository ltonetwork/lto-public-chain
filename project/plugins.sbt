resolvers ++= Seq(
  Resolver.typesafeRepo("releases"),
  Resolver.sbtPluginRepo("releases")
)

Seq(
  "com.eed3si9n"       % "sbt-assembly"             % "0.14.5",
  "com.typesafe.sbt"   % "sbt-native-packager"      % "1.5.2",
  "org.scalastyle"     %% "scalastyle-sbt-plugin"   % "1.0.0",
  "org.scoverage"      % "sbt-scoverage"            % "1.5.1",
  "se.marcuslonnberg"  % "sbt-docker"               % "1.4.1",
  "com.typesafe.sbt"   % "sbt-git"                  % "0.9.3",
  "org.scala-js"       % "sbt-scalajs"              % "0.6.33",
  "org.portable-scala" % "sbt-crossproject"         % "0.3.1",
  "org.portable-scala" % "sbt-scalajs-crossproject" % "0.3.1",
  "com.lucidchart"     % "sbt-scalafmt"             % "1.15",
  "pl.project13.scala" % "sbt-jmh"                  % "0.3.3",
).map(addSbtPlugin)

libraryDependencies ++= Seq(
  "org.vafer" % "jdeb" % "1.5" artifacts Artifact("jdeb", "jar", "jar"),
)
