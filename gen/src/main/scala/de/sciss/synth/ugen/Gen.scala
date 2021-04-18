/*
 *  Gen.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2021 Hanns Holger Rutz. All rights reserved.
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

import java.net.MalformedURLException
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.xml.XML

object Gen extends App {
  sealed trait Input {
    def mapName: String
    def switch: String
    def plugins: List[String]
  }
  case class CustomUGens(mapName: String) extends Input {
    def switch = "--custom"
    def plugins: List[String] = Nil
  }
  case object StandardUGens extends Input {
    def switch  = "--standard"
    def mapName = "StandardUGens"
    def plugins: List[String] = UGenSpec.standardPlugins
  }
  case object ThirdPartyUGens extends Input {
    def switch  = "--plugins"
    def mapName = "ThirdPartyUGens"
    def plugins: List[String] = UGenSpec.thirdPartyPlugins
  }

  case class Config(input: Input = CustomUGens("Unnamed"),
                    forceOverwrite: Boolean = false, outDir: File = new File("out"),
                    inFiles: Vec[File] = Vec.empty, docs: Boolean = true, mkMap: Boolean = true)

  object parse extends ScallopConf(args) {
    printedName = "ScalaCollider-UGens"
    version(printedName)

    val standard: Opt[Boolean]  = opt(descr = "Use standard resources as input")
    val plugins : Opt[Boolean]  = opt(descr = "Use third-party resources as input")
    val custom  : Opt[String]   = opt(descr = "Use custom resources as input. Specify map object name.")
    val force   : Opt[Boolean]  = opt(descr = "Force overwrite of output files")
    val dir     : Opt[File]     = opt(descr = "Source output root directory", required = true)
    val noDocs  : Opt[Boolean]  = opt(name = "no-docs", descr = "Do not include scaladoc comments")
    val noMap   : Opt[Boolean]  = opt(name = "no-map", descr = "Do not create mapping file")

    val input: Opt[List[File]] = trailArg[List[File]](required = false, default = Some(Nil),
      descr = "List of UGen description files (XML) to process"
    )

    verify()
    val config: Config =
      Config(input = if (standard()) StandardUGens else if (plugins()) ThirdPartyUGens else CustomUGens(custom()),
        forceOverwrite = force(), outDir = dir(), inFiles = input().toIndexedSeq, docs = !noDocs(), mkMap = !noMap()
      )
  }

  import parse.config

  import config._
  val outDir0 = config.outDir / "de" / "sciss" / "synth"
  val outDir1 = outDir0 / "ugen"
  if (!outDir1.isDirectory) if (!outDir1.mkdirs()) throw new IOException(s"Could not create directory $outDir1")

  val cg = new ClassGenerator

  val inputs: Iterator[(String, InputSource)] = input match {
    case CustomUGens(_) =>
      inFiles.iterator.map(f => f.base -> scala.xml.Source.fromFile(f))
    case _ =>
      input.plugins.iterator.map { name =>
        name -> scala.xml.Source.fromInputStream(getClass.getResourceAsStream(s"$name.xml"))
      }
  }

  val allPairs: Map[String, String] = inputs.flatMap { case (name, source) =>
    val xml = try {
      XML.load(source)
    } catch {
      case e: MalformedURLException =>
        Console.err.println(s"Resource not found: $name")
        throw e
    }
    val classNames  = cg.performFile(xml, dir = outDir1, name = name, docs = docs, forceOverwrite = forceOverwrite)
    val adjuncts    = cg.getAdjuncts(xml)
    val classPairs  = classNames.iterator.map(n => (n.replace('.', '$'), n)).toMap
    classPairs ++ adjuncts

  } .toMap

  if (config.mkMap) {
    val allPairsSq = allPairs.toIndexedSeq.sorted
    val pairs = allPairsSq.iterator.map { case (k, v) =>
      s"""    ("$k", $v),"""
    } .mkString("\n")

    val src =
      s"""package de.sciss.synth
         |
         |import UGenSource._
         |import ugen._
         |
         |object ${input.mapName} {
         |  private lazy val _init: Unit = UGenSource.addProductTypes(map)
         |
         |  def init(): Unit = _init
         |
         |  type V = ProductType[Product]
         |
         |  private def map = Map[String, V](
         |$pairs
         |  )
         |}
         |""".stripMargin

    val fMap = outDir0 / s"${input.mapName}.scala"
    cg.writeTextFile(fMap)(src)
  }

  // sys.exit()
}