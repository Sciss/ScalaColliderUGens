lazy val baseName       = "ScalaColliderUGens"
lazy val baseNameL      = baseName.toLowerCase

lazy val projectVersion = "1.21.0"
lazy val mimaVersion    = "1.21.0"

// sonatype plugin requires that these are in global
ThisBuild / version      := projectVersion
ThisBuild / organization := "de.sciss"

lazy val commonSettings = Seq(
//  version            := projectVersion,
//  organization       := "de.sciss",
  description        := "UGens for ScalaCollider",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion       := "2.13.4",
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13"),
  initialCommands in console := """import de.sciss.synth._""",
  sources in (Compile, doc) := {
    if (isDotty.value) Nil else (sources in (Compile, doc)).value // dottydoc is hopelessly broken
  },
) ++ publishSettings

lazy val commonJvmSettings = Seq(
  crossScalaVersions := Seq("3.0.0-M2", "2.13.4", "2.12.12"),
)

lazy val deps = new {
  val main = new {
    val numbers      = "0.2.1"
    val scalaXML     = "1.3.0"
    val serial       = "2.0.1"
  }
  val test = new {
    val scalaTest    = "3.2.3"
  }
  // --- gen project (not published, thus not subject to major version concerns) ---
  val gen = new {
    val fileUtil     = "1.1.5"
    val scallop      = "3.5.1"
  }
}

// ---

lazy val root = project.in(file("."))
  .aggregate(
    spec, // .jvm, spec.js,
    api.jvm, api.js,
    gen, // .jvm, // gen.js,
    core.jvm, core.js,
    plugins.jvm, plugins.js,
  )
  .settings(commonSettings)
  .settings(
    name := baseName,
    packagedArtifacts := Map.empty    // don't send this to Sonatype
  )

//// taking inspiration from http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator
//lazy val ugenGenerator = TaskKey[Seq[File]]("ugen-generate", "Generate UGen class files")

def licenseURL(licName: String, sub: String) =
  licenses := Seq(licName -> url(s"https://raw.github.com/Sciss/$baseName/main/$sub/LICENSE"))

lazy val lgpl = Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

lazy val spec = /*crossProject(JVMPlatform, JSPlatform)*/ project.in(file("spec"))
  .settings(commonSettings)
//  .jvmSettings(commonJvmSettings)
  .settings(
    name              := s"$baseName-spec",
    description       := "UGens XML specification files for ScalaCollider",
    autoScalaLibrary  := false, // this is a pure xml containing jar
    crossPaths        := false,
    licenseURL("BSD", "spec"),
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false, // there are no sources (only re-sources),
    publishArtifact := {
      val old = publishArtifact.value
      old && scalaVersion.value.startsWith("2.13")  // only publish once when cross-building
    },
    mimaPreviousArtifacts := Set("de.sciss" % s"$baseNameL-spec" % mimaVersion),
  )

lazy val api = crossProject(JVMPlatform, JSPlatform).in(file("api"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(
    name        := s"$baseName-api",
    description := "Basic UGens API for ScalaCollider",
    licenses    := lgpl,
    libraryDependencies ++= Seq(
      "de.sciss" %%% "numbers" % deps.main.numbers,
      "de.sciss" %%% "serial"  % deps.main.serial,
    ),
    libraryDependencies += {
      ("org.scala-lang.modules" %%% "scala-xml" % deps.main.scalaXML).withDottyCompat(scalaVersion.value)
    },
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) {
        case (k, opt) => k -> opt.get
      },
      BuildInfoKey.map(licenses) {
        case (_, Seq((lic, _))) => "license" -> lic
      }
    ),
    buildInfoPackage := "de.sciss.synth.ugen",
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-api" % mimaVersion)
  )

lazy val ugenGenerator = TaskKey[Unit]("ugen", "Generate UGen class files")

def genSources(base: File): File = base.getParentFile / "gen"

lazy val gen = /*crossProject(JVMPlatform)*/ project.in(file("gen"))
  .dependsOn(spec, api.jvm)
  .settings(commonSettings)
  .settings(commonJvmSettings)  // bloody sbt breaks if we decide not to use all cross versions
  .settings(
    name        := s"$baseName-gen",
    description := "Source code generator for ScalaCollider UGens",
    licenses    := lgpl,
    libraryDependencies ++= {
      if (isDotty.value) Nil else Seq(
        "de.sciss"        %% "fileutil"       % deps.gen.fileUtil,
        "org.scala-lang"  %  "scala-compiler" % scalaVersion.value,
        "org.rogach"      %% "scallop"        % deps.gen.scallop,
        "org.scalatest"   %% "scalatest"      % deps.test.scalaTest % Test
      )
    },
    unmanagedSources / excludeFilter := {
      val sv = scalaVersion.value
      // yeah, well, sbt-crossproject bullshit.
      // we cannot remove `crossScalaVersion` "because",
      // and this seems to be the only way to prevent anything
      // compiling under 2.12 or 3.0
      if (sv.startsWith("2.13.")) "*.unused" else "*.scala"
    },
    ugenGenerator in Compile := {
      val pairs = Seq[(File, String)](
        ((baseDirectory in Compile in core    .jvm).value, "--standard"),
        ((baseDirectory in Compile in plugins .jvm).value, "--plugins" ),
      )
      pairs.foreach { case (base, arg) =>
        val src = genSources(base)
        val cp = (fullClasspath in Compile).value
        val st = streams.value
        runUGenGenerator(description.value, outputDir = src, cp = cp.files, log = st.log, args = arg :: Nil)
      }
    },
    //    ugenGenerator in Compile := {
    //      val src   = (sourceManaged       in Compile       ).value
    //      val cp    = (dependencyClasspath in Runtime in gen.jvm).value
    //      val st    = streams.value
    //      runUGenGenerator(description.value, outputDir = src, cp = cp.files, log = st.log, args = "--plugins" :: Nil)
    //    },
    mimaPreviousArtifacts := Set.empty,
    publishLocal    := {},
    publish         := {},
    publishArtifact := false,   // cf. http://stackoverflow.com/questions/8786708/
    publishTo       := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )

lazy val core = crossProject(JVMPlatform, JSPlatform).in(file("core"))
  .dependsOn(api)
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(
    name        := s"$baseName-core",
    description := "Standard UGens",
    licenses    := lgpl,
//    Compile / sourceGenerators += ugenGenerator in Compile,
//    ugenGenerator in Compile := {
//      val src   = (sourceManaged       in Compile       ).value
//      val cp    = (dependencyClasspath in Runtime in gen.jvm).value
//      val st    = streams.value
//      runUGenGenerator(description.value, outputDir = src, cp = cp.files, log = st.log, args = "--standard" :: Nil)
//    },
    Compile / unmanagedSourceDirectories += genSources(baseDirectory.value),
//    mappings in (Compile, packageSrc) ++= {
//      val base  = (sourceManaged  in Compile).value
//      val files = (managedSources in Compile).value
//      files.map { f => (f, f.relativeTo(base).get.getPath) }
//    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )

lazy val plugins = crossProject(JVMPlatform, JSPlatform).in(file("plugins"))
  .dependsOn(core)
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(
    name        := s"$baseName-plugins",
    description := "Additional third-party UGens",
    licenses    := lgpl,
//    Compile / sourceGenerators += ugenGenerator in Compile,
//    ugenGenerator in Compile := {
//      val src   = (sourceManaged       in Compile       ).value
//      val cp    = (dependencyClasspath in Runtime in gen.jvm).value
//      val st    = streams.value
//      runUGenGenerator(description.value, outputDir = src, cp = cp.files, log = st.log, args = "--plugins" :: Nil)
//    },
    Compile / unmanagedSourceDirectories += genSources(baseDirectory.value),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-plugins" % mimaVersion)
  )

/** @param name       purely informational string emitted through the sbt log
  * @param outputDir  target directory, e.g. `sourceManaged`
  * @param cp         dependency classpath
  * @param log        such as `streams.value.log`
  * @param args       the arguments to pass to the `Gen` main program.
  *                   this can be a simple switch to select ugens, such
  *                   as `--standard` or `--plugins`, or a list of paths
  *                   to XML descriptions.
  */
def runUGenGenerator(name: String, outputDir: File, cp: Seq[File], log: Logger,
                     args: Seq[String]): Unit = {
  val mainClass   = "de.sciss.synth.ugen.Gen"
  val tmp         = java.io.File.createTempFile("sources", ".txt")
  val os          = new java.io.FileOutputStream(tmp)
  outputDir.mkdirs()
  val oldFiles = (outputDir ** "*.scala").get()
  oldFiles.foreach(_.delete())

  log.info(s"Generating UGen source code in $outputDir for $name")

  try {
    val outs  = CustomOutput(os)
    val fOpt  = ForkOptions(
      javaHome          = Option.empty[File],
      workingDirectory  = Option.empty[File],
      connectInput      = false,
      outputStrategy    = Some(outs),
      bootJars          = cp.toVector,
      runJVMOptions     = Vector.empty[String],
      envVars           = Map.empty[String, String],
    )
    val res: Int =
      Fork.scala(config = fOpt,
      arguments = mainClass :: "-d" :: outputDir.getAbsolutePath :: args.toList)

    if (res != 0) {
      sys.error(s"UGen class file generator failed with exit code $res")
    }
  } finally {
    os.close()
  }
//  val sources = {
//    val src = scala.io.Source.fromFile(tmp)
//    try {
//      src.getLines().map(file).toList
//    } finally {
//      src.close()
//    }
//  }

  tmp.delete()
//  sources
}

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  developers := List(
    Developer(
      id    = "sciss",
      name  = "Hanns Holger Rutz",
      email = "contact@sciss.de",
      url   = url("https://www.sciss.de")
    )
  ),
  scmInfo := {
    val h = "git.iem.at"
    val a = s"sciss/$baseName"
    Some(ScmInfo(url(s"https://$h/$a"), s"scm:git@$h:$a.git"))
  },
)


