import com.typesafe.sbt.packager.archetypes.TemplateWriter
import sbt.Keys.{sourceGenerators, _}
import sbt._
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbt.internal.inc.ReflectUtilities
import sbtassembly.MergeStrategy

enablePlugins(JavaServerAppPackaging, JDebPackaging, SystemdPlugin, GitVersioning)
ThisBuild / scalafmtOnCompile := true

val versionSource = Def.task {
  val versionFile      = (Compile / sourceManaged).value / "com" / "ltonetwork" / "Version.scala"
  val versionExtractor = """(\d+)\.(\d+)\.(\d+).*""".r

  version.value match {
    case versionExtractor(ma, mi, pa) =>
      val (major, minor, patch) = (ma.toInt, mi.toInt, pa.toInt)
      IO.write(
        versionFile,
        s"""package com.ltonetwork
           |
           |object Version {
           |  val VersionString = "${version.value}"
           |  val VersionTuple = ($major, $minor, $patch)
           |}
           |""".stripMargin
      )
      Seq(versionFile)
    case _ => Seq()
  }
}
val network = SettingKey[Network]("network")
network := { Network(sys.props.get("network")) }
name := "lto"
normalizedName := s"${name.value}${network.value.packageSuffix}"
unmanagedSources / excludeFilter := "Version.scala"

git.useGitDescribe := true
git.uncommittedSignifier := Some("DIRTY")
logBuffered := false

inThisBuild(
  Seq(
    scalaVersion := "2.12.9",
    organization := "com.ltonetwork",
    crossPaths := false,
    scalacOptions ++= Seq("-feature", "-deprecation", "-language:higherKinds", "-language:implicitConversions", "-Ywarn-unused:-implicits")
  ))

resolvers += Resolver.file("local-dependencies", file("dependencies"))(Resolver.ivyStylePatterns)

run / fork := true
run / javaOptions ++= Seq(
  "-XX:+IgnoreUnrecognizedVMOptions",
  "--add-modules=java.xml.bind"
)

val aopMerge: MergeStrategy = new MergeStrategy {
  val name = "aopMerge"
  import scala.xml._
  import scala.xml.dtd._

  def apply(tempDir: File, path: String, files: Seq[File]): Either[String, Seq[(File, String)]] = {
    val dt                         = DocType("aspectj", PublicID("-//AspectJ//DTD//EN", "http://www.eclipse.org/aspectj/dtd/aspectj.dtd"), Nil)
    val file                       = MergeStrategy.createMergeTarget(tempDir, path)
    val xmls: Seq[Elem]            = files.map(XML.loadFile)
    val aspectsChildren: Seq[Node] = xmls.flatMap(_ \\ "aspectj" \ "aspects" \ "_")
    val weaverChildren: Seq[Node]  = xmls.flatMap(_ \\ "aspectj" \ "weaver" \ "_")
    val options: String            = xmls.map(x => (x \\ "aspectj" \ "weaver" \ "@options").text).mkString(" ").trim
    val weaverAttr                 = if (options.isEmpty) Null else new UnprefixedAttribute("options", options, Null)
    val aspects                    = new Elem(null, "aspects", Null, TopScope, false, aspectsChildren: _*)
    val weaver                     = new Elem(null, "weaver", weaverAttr, TopScope, false, weaverChildren: _*)
    val aspectj                    = new Elem(null, "aspectj", Null, TopScope, false, aspects, weaver)
    XML.save(file.toString, aspectj, "UTF-8", xmlDecl = false, dt)
    IO.append(file, IO.Newline.getBytes(IO.defaultCharset))
    Right(Seq(file -> path))
  }
}

inTask(assembly)(
  Seq(
    test := {},
    assemblyJarName := s"lto-public-all-${version.value}.jar",
    assemblyMergeStrategy := {
      case PathList("META-INF", "io.netty.versions.properties")                     => MergeStrategy.concat
      case PathList("META-INF", "aop.xml")                                          => aopMerge
      case PathList(xs @ _*) if xs.lastOption.fold(false)(_ == "module-info.class") => MergeStrategy.discard
      case other                                                                    => (assembly / assemblyMergeStrategy).value(other)
    }
  ))

inConfig(Compile)(
  Seq(
    mainClass := Some("com.ltonetwork.Application"),
    packageDoc / publishArtifact := false,
    packageSrc / publishArtifact := false,
    sourceGenerators += versionSource
  ))

inConfig(Test)(
  Seq(
    logBuffered := false,
    parallelExecution := false,
    testOptions += Tests.Argument("-oIDOF", "-u", "target/test-reports"),
    testOptions += Tests.Setup(_ => sys.props("sbt-testing") = "true")
  ))

inConfig(Linux)(
  Seq(
    maintainer := "lto.network",
    packageSummary := "LTO Network node",
    packageDescription := "LTO Network node"
  ))

bashScriptExtraDefines += s"""addJava "-lto.directory=/var/lib/${normalizedName.value}""""

val linuxScriptPattern = "bin/(.+)".r
val batScriptPattern   = "bin/([^.]+)\\.bat".r

inConfig(Universal)(
  Seq(
    mappings += (baseDirectory.value / s"lto-${network.value}.conf" -> "doc/lto.conf.sample"),
    mappings := {
      val scriptSuffix = network.value.packageSuffix
      mappings.value.map {
        case m @ (file, batScriptPattern(script)) =>
          if (script.endsWith(scriptSuffix)) m else (file, s"bin/$script$scriptSuffix.bat")
        case m @ (file, linuxScriptPattern(script)) =>
          if (script.endsWith(scriptSuffix)) m else (file, s"bin/$script$scriptSuffix")
        case other => other
      }
    },
    javaOptions ++= Seq(
      // -J prefix is required by the bash script
      "-J-server",
      // JVM memory tuning for 2g ram
      "-J-Xms128m",
      "-J-Xmx2g",
      "-J-XX:+ExitOnOutOfMemoryError",
      "-J-XX:+IgnoreUnrecognizedVMOptions",
      // from https://groups.google.com/d/msg/akka-user/9s4Yl7aEz3E/zfxmdc0cGQAJ
      "-J-XX:+UseG1GC",
      "-J-XX:+UseNUMA",
      "-J-XX:+AlwaysPreTouch",
      // probably can't use these with jstack and others tools
      "-J-XX:+PerfDisableSharedMem",
      "-J-XX:+ParallelRefProcEnabled",
      "-J-XX:+UseStringDeduplication"
    )
  ))

val packageSource = Def.setting {
  sourceDirectory.value / "package"
}

val upstartScript = Def.task {
  val src    = packageSource.value / "upstart.conf"
  val dest   = (Debian / target).value / "upstart" / s"${packageName.value}.conf"
  val result = TemplateWriter.generateScript(src.toURI.toURL, linuxScriptReplacements.value)
  IO.write(dest, result)
  dest
}

linuxPackageMappings ++= Seq(
  (upstartScript.value, s"/etc/init/${packageName.value}.conf")
).map(packageMapping(_).withConfig().withPerms("644"))

linuxScriptReplacements += "detect-loader" ->
  """is_systemd() {
    |    which systemctl >/dev/null 2>&1 && \
    |    systemctl | grep -- -\.mount >/dev/null 2>&1
    |}
    |is_upstart() {
    |    /sbin/init --version | grep upstart >/dev/null 2>&1
    |}
    |""".stripMargin

inConfig(Debian)(
  Seq(
    linuxStartScriptTemplate := (packageSource.value / "systemd.service").toURI.toURL,
    debianPackageDependencies += "java11-runtime-headless",
    serviceAutostart := false,
    maintainerScripts := maintainerScriptsFromDirectory(packageSource.value / "debian", Seq("preinst", "postinst", "postrm", "prerm"))
  ))

commands += Command.command("build") { state =>
  "clean" :: "assembly" :: state
}

commands += Command.command("packageAll") { state =>
  "assembly" :: "debian:packageBin" :: state
}

// https://stackoverflow.com/a/48592704/4050580
def allProjects: List[ProjectReference] = ReflectUtilities.allVals[Project](this).values.toList map { p =>
  p: ProjectReference
}

addCommandAlias("checkPR", """; Global / checkPRRaw""")
lazy val checkPRRaw = taskKey[Unit]("Build a project and run unit tests")
checkPRRaw in Global := {
  try {
    clean.all(ScopeFilter(inProjects(allProjects: _*), inConfigurations(Compile))).value
  } finally {
    test.all(ScopeFilter(inProjects(langJVM, node), inConfigurations(Test))).value
    compile.all(ScopeFilter(inProjects(generator, benchmark), inConfigurations(Test))).value
  }
}

lazy val lang =
  crossProject(JSPlatform, JVMPlatform)
    .withoutSuffixFor(JVMPlatform)
    .settings(
      version := "0.0.1",
      // the following line forces scala version across all dependencies
      scalaModuleInfo ~= (_.map(_.withOverrideScalaVersion(true))),
      assembly / test := {},
      addCompilerPlugin(Dependencies.kindProjector),
      libraryDependencies ++=
        Dependencies.cats ++
          Dependencies.fp ++
          Dependencies.scalacheck ++
          Dependencies.scorex ++
          Dependencies.scalatest.map(_ % "test") ++
          Dependencies.scalactic ++
          Dependencies.monix.value ++
          Dependencies.scodec.value ++
          Dependencies.fastparse.value ++
          Dependencies.seasalt,
      resolvers += Resolver.bintrayIvyRepo("portable-scala", "sbt-plugins")
    )
    .jsSettings(
      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      }
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.scala-js"                %% "scalajs-stubs" % "0.6.22" % "provided"
      ) ++ Dependencies.logging.map(_ % "test") // scrypto logs an error if a signature verification was failed
    )

lazy val langJS  = lang.js
lazy val langJVM = lang.jvm

lazy val node = project
  .in(file("."))
  .settings(
    addCompilerPlugin(Dependencies.kindProjector),
    libraryDependencies ++=
      Dependencies.network ++
        Dependencies.db ++
        Dependencies.http ++
        Dependencies.akka ++
        Dependencies.serialization ++
        Dependencies.testKit.map(_ % "test") ++
        Dependencies.logging ++
        Dependencies.matcher ++
        Dependencies.metrics ++
        Dependencies.fp ++
        Dependencies.meta ++
        Dependencies.ficus ++
        Dependencies.scorex ++
        Dependencies.commons_net ++
        Dependencies.jaxb_api ++
        Dependencies.monix.value
  )
  .dependsOn(langJVM)

lazy val it = project
  .dependsOn(node)

lazy val generator = project
  .dependsOn(it)
  .settings(libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0")

lazy val benchmark = project
  .enablePlugins(JmhPlugin)
  .dependsOn(node % "compile->compile;test->test", langJVM % "compile->compile;test->test")
