/*
 *  Gen.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2018 Hanns Holger Rutz. All rights reserved.
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
import org.xml.sax.InputSource
import scopt.OptionParser

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

  val parser  = new OptionParser[Config]("ScalaCollider-UGens") {
    opt[Unit]("standard"      ) text "Use standard resources as input"  action { (_, c) => c.copy(input          = StandardUGens  ) }
    opt[Unit]("plugins"       ) text "Use third-party resources as input" action{(_, c) => c.copy(input          = ThirdPartyUGens) }
    opt[Unit]('f', "force"    ) text "Force overwrite of output files"  action { (_, c) => c.copy(forceOverwrite = true ) }
    opt[File]('d', "dir"      ) text "Source output root directory"     action { (f, c) => c.copy(outDir         = f    ) }
    opt[Unit]("no-docs"       ) text "Do not include scaladoc comments" action { (_, c) => c.copy(docs           = false) }

    // help("help") text "prints this usage text"

    arg[File]("<input>...") unbounded() optional() text "List of UGen description files (XML) to process" action {
      (f, c) => c.copy(inFiles = c.inFiles :+ f)
    }
  }

  val config  = parser.parse(args, Config()) getOrElse sys.exit(1)

  import config._
  val outDir1 = config.outDir / "de" / "sciss" / "synth" / "ugen"
  if (!outDir1.isDirectory) if (!outDir1.mkdirs()) throw new IOException(s"Could not create directory $outDir1")

  val synth = new ClassGenerator

  val inputs: Iterator[(String, InputSource)] = if (input == CustomUGens) {
    inFiles.iterator.map(f => f.base -> xml.Source.fromFile(f))
  } else {
    input.plugins.iterator.map { name =>
      name -> xml.Source.fromInputStream(getClass.getResourceAsStream(s"$name.xml"))
    }
  }

  inputs.foreach { case (name, source) =>
    val xml = XML.load(source)
    synth.performFile(xml, dir = outDir1, name = name, docs = docs, forceOverwrite = forceOverwrite)
  }
  // sys.exit()
}