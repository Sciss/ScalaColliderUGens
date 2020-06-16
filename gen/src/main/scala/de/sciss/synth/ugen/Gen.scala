/*
 *  Gen.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import java.io.IOException

import de.sciss.file._
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}
import org.xml.sax.InputSource

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.xml.XML

object Gen extends App {
  sealed trait Input {
    def switch: String
    def plugins: List[String]
  }
  case object CustomUGens extends Input {
    def switch = ""
    def plugins: List[String] = Nil
  }
  case object StandardUGens extends Input {
    def switch = "--standard"
    def plugins: List[String] = UGenSpec.standardPlugins
  }
  case object ThirdPartyUGens extends Input {
    def switch = "--plugins"
    def plugins: List[String] = UGenSpec.thirdPartyPlugins
  }

  case class Config(input: Input = CustomUGens, forceOverwrite: Boolean = false, outDir: File = new File("out"),
                    inFiles: Vec[File] = Vec.empty, docs: Boolean = true)

  object parse extends ScallopConf(args) {
    printedName = "ScalaCollider-UGens"
    version(printedName)

    val standard: Opt[Boolean]  = opt(descr = "Use standard resources as input")
    val plugins : Opt[Boolean]  = opt(descr = "Use third-party resources as input")
    val force   : Opt[Boolean]  = opt(descr = "Force overwrite of output files")
    val dir     : Opt[File]     = opt(descr = "Source output root directory", required = true)
    val noDocs  : Opt[Boolean]  = opt(name = "no-docs", descr = "Do not include scaladoc comments")

    val input: Opt[List[File]] = trailArg[List[File]](required = false, default = Some(Nil),
      descr = "List of UGen description files (XML) to process"
    )

    verify()
    val config = Config(input = if (standard()) StandardUGens else if (plugins()) ThirdPartyUGens else CustomUGens,
      forceOverwrite = force(), outDir = dir(), inFiles = input().toIndexedSeq, docs = !noDocs()
    )
  }

  import parse.config

  import config._
  val outDir1 = config.outDir / "de" / "sciss" / "synth" / "ugen"
  if (!outDir1.isDirectory) if (!outDir1.mkdirs()) throw new IOException(s"Could not create directory $outDir1")

  val synth = new ClassGenerator

  val inputs: Iterator[(String, InputSource)] = if (input == CustomUGens) {
    inFiles.iterator.map(f => f.base -> scala.xml.Source.fromFile(f))
  } else {
    input.plugins.iterator.map { name =>
      name -> scala.xml.Source.fromInputStream(getClass.getResourceAsStream(s"$name.xml"))
    }
  }

  inputs.foreach { case (name, source) =>
    val xml = XML.load(source)
    synth.performFile(xml, dir = outDir1, name = name, docs = docs, forceOverwrite = forceOverwrite)
  }
  // sys.exit()
}