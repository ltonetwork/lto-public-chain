import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.toPlatformDepsGroupID
import sbt._

//noinspection TypeAnnotation,ScalaStyle
object Dependencies {

  val excludeScalaTest = ExclusionRule(organization = "org.scalatest")
  val asyncHttpClient = "org.asynchttpclient" % "async-http-client" % "2.4.7"

  def akkaModule(module: String) = "com.typesafe.akka" %% s"akka-$module" % "2.6.16" excludeAll(excludeScalaTest)

  def swaggerModule(module: String) = "io.swagger.core.v3" % s"swagger-$module-jakarta" % "2.1.11" exclude("com.google.guava", "guava")

  def akkaHttpModule(module: String) = "com.typesafe.akka" %% module % "10.2.7" excludeAll(excludeScalaTest)

  def nettyModule(module: String) = "io.netty" % s"netty-$module" % "4.1.24.Final"

  def kamonModule(v: String)(module: String) = "io.kamon" %% s"kamon-$module" % v

  lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.6"
  
  lazy val network = Seq("handler", "buffer", "codec").map(nettyModule) ++ Seq(
    "org.bitlet" % "weupnp" % "0.1.4",
    // Solves an issue with kamon-influxdb
    asyncHttpClient.exclude("io.netty", "netty-handler")
  )

  lazy val testKit = scalatest ++ Seq(
    akkaModule("testkit"),
    "org.scalacheck"   %% "scalacheck"                  % "1.13.5",
    "org.mockito"      %  "mockito-all"                 % "1.10.19",
    "org.scalamock"    %% "scalamock-scalatest-support" % "3.6.0",
    "org.iq80.leveldb" %  "leveldb"                     % "0.9" exclude("com.google.guava", "guava"),
    akkaHttpModule("akka-http-testkit")
  ).map(_ % "test")

  lazy val itKit = scalatest ++ Seq(
    // Swagger is using Jersey 1.1, hence the shading (https://github.com/spotify/docker-client#a-note-on-shading)
    "com.spotify" % "docker-client" % "8.11.3" classifier("shaded") exclude("com.google.guava", "guava"),
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-properties" % "2.9.5",
    asyncHttpClient.exclude("io.netty", "netty-handler")
  )

  lazy val serialization = Seq(
    "com.google.guava"  % "guava"         % "21.0",
    "com.google.guava"  % "failureaccess" % "1.0.1",
    "com.typesafe.play" %% "play-json"    % "2.6.9"
  )
  lazy val akka = Seq("actor", "slf4j", "actor-typed", "serialization-jackson").map(akkaModule)

  lazy val db = Seq(
    "org.ethereum" % "leveldbjni-all" % "1.18.3"
  )

  lazy val logging = Seq(
    "ch.qos.logback"       % "logback-classic"          % "1.2.3",
    "org.slf4j"            % "slf4j-api"                % "1.7.25",
    "org.slf4j"            % "jul-to-slf4j"             % "1.7.25",
    "net.logstash.logback" % "logstash-logback-encoder" % "4.11"
  )

  lazy val http = Seq("core", "annotations", "models", "jaxrs2", "jaxrs2-servlet-initializer-v2").map(swaggerModule) ++ Seq(
    "com.github.swagger-akka-http" %% "swagger-scala-module" % "2.5.2" excludeAll(excludeScalaTest),
    "com.github.swagger-akka-http" %% "swagger-akka-http"    % "2.6.0" excludeAll(excludeScalaTest),
    "jakarta.ws.rs"                 % "jakarta.ws.rs-api"    % "3.0.0" excludeAll(excludeScalaTest),
      akkaHttpModule("akka-http")
  )

  lazy val matcher = Seq(
    akkaModule("persistence"),
    akkaModule("persistence-tck") % "test",
    "com.github.dnvriend" %% "akka-persistence-inmemory" % "2.4.18.1" % "test",
    "org.ethereum" % "leveldbjni-all" % "1.18.3"
  )

  lazy val metrics = {
    Seq("core", "system-metrics").map(kamonModule("0.6.7")) ++
      Seq("akka-2.4", "influxdb").map(kamonModule("0.6.8")) ++
      Seq(
        "org.influxdb" % "influxdb-java"    % "2.7",
        "io.kamon"     %% "kamon-autoweave" % "0.6.5"
      )
  }.map(_.exclude("org.asynchttpclient", "async-http-client"))

  lazy val fp = Seq(
    "org.typelevel"       %% "cats-core"       % "1.1.0" excludeAll(excludeScalaTest),
    "org.typelevel"       %% "cats-mtl-core"   % "0.2.1" excludeAll(excludeScalaTest),
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0" % "test"
  )
  lazy val meta        = Seq("com.chuusai" %% "shapeless" % "2.3.3")
  lazy val monix       = Def.setting(Seq(
    // exclusion and explicit dependency can likely be removed when monix 3 is released
    "io.monix" %%% "monix" % "3.0.0-RC1" exclude("org.typelevel", "cats-effect_2.12"),
    "org.typelevel" %%% "cats-effect"  % "0.10.1"
  ))
  lazy val scodec      = Def.setting(Seq("org.scodec" %%% "scodec-core" % "1.10.3"))
  lazy val fastparse   = Def.setting(Seq(
    "com.lihaoyi" %%% "fastparse" % "1.0.0",
    "org.bykn" %%% "fastparse-cats-core" % "0.1.0" excludeAll(excludeScalaTest)
  ))
  lazy val ficus       = Seq("com.iheart" %% "ficus" % "1.4.2")
  lazy val scorex      = Seq("org.scorexfoundation" %% "scrypto" % "2.0.4" excludeAll(
    ExclusionRule("org.slf4j", "slf4j-api"),
    ExclusionRule("com.google.guava", "guava")
  ))
  lazy val commons_net = Seq("commons-net" % "commons-net" % "3.+")
  lazy val scalatest   = Seq(
    "org.scalatest"          %% "scalatest" % "3.1.4" % "test",
    "org.scalatestplus"      %% "scalacheck-1-14" % "3.2.0.0" % "test",
    "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % "test"
  )
  lazy val scalactic   = Seq("org.scalactic" %% "scalactic" % "3.1.4")
  lazy val cats        = Seq("org.typelevel" %% "cats-core" % "1.1.0" excludeAll(excludeScalaTest))
  lazy val scalacheck = Seq(
    "org.scalacheck"      %% "scalacheck"      % "1.13.5" % "test",
    "io.github.amrhassan" %% "scalacheck-cats" % "0.4.0"  % "test"
  )

  lazy val seasalt = Seq("com.ltonetwork" % "seasalt" % "0.0.10")
  lazy val jaxb_api = Seq(
    "javax.xml.bind" % "jaxb-api" % "2.3.0",
    "com.sun.xml.bind" % "jaxb-core" % "2.3.0",
    "com.sun.xml.bind" % "jaxb-impl" % "2.3.0"
  )
  lazy val bouncycastle = Seq("org.bouncycastle" % "bcpkix-jdk15to18" % "1.69")

}
