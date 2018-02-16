lazy val baseName       = "ScalaColliderUGens"
lazy val baseNameL      = baseName.toLowerCase

lazy val projectVersion = "1.17.1"
lazy val mimaVersion    = "1.17.1"

name := baseName

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  description        := "UGens for ScalaCollider",
  homepage           := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion       := "2.13.0-M3",
  crossScalaVersions := Seq("2.12.4", "2.11.12"),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  initialCommands in console := """import de.sciss.synth._"""
) ++ publishSettings

lazy val deps = new {
  val main = new {
    val numbers      = "0.1.4"
    val xml          = "1.0.6"
  }
  val test = new {
    val scalatest    = "3.0.5-M1"
  }
  val gen = new {
    // --- gen project (not published, thus not subject to major version concerns) ---
    val fileutil     = "1.1.3"
    val scopt        = "3.7.0"
  }
}

// ---

lazy val root = Project(id = baseNameL, base = file(".")).
  aggregate(spec, api, gen, core, plugins).
  settings(commonSettings).
  settings(
    packagedArtifacts := Map.empty    // don't send this to Sonatype
  )

// taking inspiration from http://stackoverflow.com/questions/11509843/sbt-generate-code-using-project-defined-generator
lazy val ugenGenerator = TaskKey[Seq[File]]("ugen-generate", "Generate UGen class files")

def licenseURL(licName: String, sub: String) =
  licenses := Seq(licName -> url(s"https://raw.github.com/Sciss/$baseName/master/$sub/LICENSE"))

lazy val lgpl = Seq("LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt"))

lazy val spec = Project(id = s"$baseNameL-spec", base = file("spec")).
  settings(commonSettings).
  settings(
    description := "UGens XML specification files for ScalaCollider",
    autoScalaLibrary := false, // this is a pure xml containing jar
    crossPaths := false,
    licenseURL("BSD", "spec"),
    publishArtifact in (Compile, packageDoc) := false, // there are no javadocs
    publishArtifact in (Compile, packageSrc) := false, // there are no sources (only re-sources)
    mimaPreviousArtifacts := Set("de.sciss" % s"$baseNameL-spec" % mimaVersion)
  )

lazy val api = Project(id = s"$baseNameL-api", base = file("api")).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings).
  settings(
    description := "Basic UGens API for ScalaCollider",
    licenses := lgpl,
    libraryDependencies ++= Seq(
      "de.sciss"               %% "numbers"   % deps.main.numbers,
      "org.scala-lang.modules" %% "scala-xml" % deps.main.xml
    ),
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

lazy val gen = Project(id = s"$baseNameL-gen", base = file("gen")).
  dependsOn(spec, api).
  settings(commonSettings).
  settings(
    description := "Source code generator for ScalaCollider UGens",
    licenses := lgpl,
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt"     % deps.gen.scopt,
      "de.sciss"         %% "fileutil"  % deps.gen.fileutil,
      "org.scala-lang"   %  "scala-compiler" % scalaVersion.value,
      "org.scalatest"    %% "scalatest" % deps.test.scalatest % "test"
    ),
    mimaPreviousArtifacts := Set.empty,
    publishLocal    := {},
    publish         := {},
    publishArtifact := false,   // cf. http://stackoverflow.com/questions/8786708/
    publishTo       := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
  )

lazy val core = Project(id = s"$baseNameL-core", base = file("core")).
  dependsOn(api).
  settings(commonSettings).
  settings(
    description := "Standard UGens",
    licenses := lgpl,
    sourceGenerators in Compile <+= ugenGenerator in Compile,
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

lazy val plugins = Project(id = s"$baseNameL-plugins", base = file("plugins")).
  dependsOn(core).
  settings(commonSettings).
  settings(
    description := "Additional third-party UGens",
    licenses := lgpl,
    sourceGenerators in Compile <+= ugenGenerator in Compile,
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
    val fOpt  = ForkOptions(javaHome = None, outputStrategy = Some(outs), /* runJVMOptions = Nil, */ bootJars = cp,
        workingDirectory = None, connectInput = false)
    val res: Int = Fork.scala(config = fOpt,
      arguments = mainClass :: "-d" :: outputDir.getAbsolutePath :: args.toList)

    if (res != 0) {
      sys.error(s"UGen class file generator failed with exit code $res")
    }
  } finally {
    os.close()
  }
  val sources = scala.io.Source.fromFile(tmp).getLines().map(file).toList
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
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
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
