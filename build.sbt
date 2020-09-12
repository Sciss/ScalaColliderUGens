lazy val baseName       = "ScalaColliderUGens"
lazy val baseNameL      = baseName.toLowerCase

lazy val projectVersion = "1.19.8-SNAPSHOT"
lazy val mimaVersion    = "1.19.0"

name := baseName

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  description        := "UGens for ScalaCollider",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion       := "2.13.3",
  crossScalaVersions := Seq("0.27.0-RC1", "2.13.3", "2.12.12"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13"),
  initialCommands in console := """import de.sciss.synth._"""
) ++ publishSettings

lazy val deps = new {
  val main = new {
    val numbers      = "0.2.1"
    val scalaXML     = "1.2.0" // "1.0.6" // scala-compiler 2.11 and 2.12 use 1.0.x, but other libraries now go for this version, catch-22
  }
  val test = new {
    val scalaTest    = "3.2.2"
  }
  // --- gen project (not published, thus not subject to major version concerns) ---
  val gen = new {
    val fileUtil     = "1.1.5"
    val scallop      = "3.5.1"
  }
}

// ---

lazy val root = project.withId(baseNameL).in(file("."))
  .aggregate(spec, api, gen, core, plugins)
  .settings(commonSettings)
  .settings(
    packagedArtifacts := Map.empty    // don't send this to Sonatype
  )

// taking inspiration from http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator
lazy val ugenGenerator = TaskKey[Seq[File]]("ugen-generate", "Generate UGen class files")

def licenseURL(licName: String, sub: String) =
  licenses := Seq(licName -> url(s"https://raw.github.com/Sciss/$baseName/main/$sub/LICENSE"))

lazy val lgpl = Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

lazy val spec = project.withId(s"$baseNameL-spec").in(file("spec"))
  .settings(commonSettings)
  .settings(
    description := "UGens XML specification files for ScalaCollider",
    autoScalaLibrary := false, // this is a pure xml containing jar
    crossPaths := false,
    licenseURL("BSD", "spec"),
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false, // there are no sources (only re-sources),
    publishArtifact := {
      val old = publishArtifact.value
      old && scalaVersion.value.startsWith("2.12")  // only publish once when cross-building
    },
    mimaPreviousArtifacts := Set("de.sciss" % s"$baseNameL-spec" % mimaVersion)
  )

lazy val api = project.withId(s"$baseNameL-api").in(file("api"))
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    description := "Basic UGens API for ScalaCollider",
    licenses := lgpl,
    libraryDependencies ++= Seq(
      "de.sciss" %% "numbers" % deps.main.numbers,
    ),
    libraryDependencies += {
      "org.scala-lang.modules"  %% "scala-xml" % deps.main.scalaXML
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

lazy val gen = project.withId(s"$baseNameL-gen").in(file("gen"))
  .dependsOn(spec, api)
  .settings(commonSettings)
  .settings(
    description := "Source code generator for ScalaCollider UGens",
    licenses := lgpl,
    libraryDependencies ++= Seq(
      "de.sciss"        %% "fileutil"       % deps.gen.fileUtil,
      "org.scala-lang"  %  "scala-compiler" % scalaVersion.value,
      "org.rogach"      %% "scallop"        % deps.gen.scallop,
      "org.scalatest"   %% "scalatest"      % deps.test.scalaTest % Test
    ),
    mimaPreviousArtifacts := Set.empty,
    publishLocal    := {},
    publish         := {},
    publishArtifact := false,   // cf. http://stackoverflow.com/questions/8786708/
    publishTo       := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )

lazy val core = project.withId(s"$baseNameL-core").in(file("core"))
  .dependsOn(api)
  .settings(commonSettings)
  .settings(
    description := "Standard UGens",
    licenses := lgpl,
    Compile / sourceGenerators += ugenGenerator in Compile,
    ugenGenerator in Compile := {
      val src   = (sourceManaged       in Compile        ).value
      val cp    = (dependencyClasspath in Runtime in gen ).value
      val st    = streams.value
      runUGenGenerator(description.value, outputDir = src, cp = cp.files, log = st.log, args = "--standard" :: Nil)
    },
    mappings in (Compile, packageSrc) ++= {
      val base  = (sourceManaged  in Compile).value
      val files = (managedSources in Compile).value
      files.map { f => (f, f.relativeTo(base).get.getPath) }
    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-core" % mimaVersion)
  )

lazy val plugins = project.withId(s"$baseNameL-plugins").in(file("plugins"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    description := "Additional third-party UGens",
    licenses := lgpl,
    Compile / sourceGenerators += ugenGenerator in Compile,
    ugenGenerator in Compile := {
      val src   = (sourceManaged       in Compile        ).value
      val cp    = (dependencyClasspath in Runtime in gen ).value
      val st    = streams.value
      runUGenGenerator(description.value, outputDir = src, cp = cp.files, log = st.log, args = "--plugins" :: Nil)
    },
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
  * @return           the list of generated source files
  */
def runUGenGenerator(name: String, outputDir: File, cp: Seq[File], log: Logger,
                     args: Seq[String]): Seq[File] = {
  val mainClass   = "de.sciss.synth.ugen.Gen"
  val tmp         = java.io.File.createTempFile("sources", ".txt")
  val os          = new java.io.FileOutputStream(tmp)

  log.info(s"Generating UGen source code in $outputDir for $name")

  try {
    val outs  = CustomOutput(os)
    val fOpt  = ForkOptions(javaHome = Option.empty[File], outputStrategy = Some(outs), bootJars = cp.toVector,
      workingDirectory = Option.empty[File], runJVMOptions = Vector.empty[String], connectInput = false, envVars = Map.empty[String, String])
    val res: Int = Fork.scala(config = fOpt,
      arguments = mainClass :: "-d" :: outputDir.getAbsolutePath :: args.toList)

    if (res != 0) {
      sys.error(s"UGen class file generator failed with exit code $res")
    }
  } finally {
    os.close()
  }
  val sources = {
    val src = scala.io.Source.fromFile(tmp)
    try {
      src.getLines().map(file).toList
    } finally {
      src.close()
    }
  }
  tmp.delete()
  sources
}

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = baseName
<scm>
  <url>git@git.iem.at:sciss/{n}.git</url>
  <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
  }
)
