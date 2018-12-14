/*
 *  UGenGraph.scala
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

import java.io.{DataInputStream, DataOutputStream}
import java.nio.charset.Charset

import de.sciss.synth.ugen.impl.RawUGenImpl

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}

object UGenGraph {
  private final val v1Rd  = { dis: DataInputStream => dis.readShort().toInt }
  private final val v2Rd  = { dis: DataInputStream => dis.readInt  ()       }
  private final val v1Wr  = { (dos: DataOutputStream, value: Int) =>
    if (value < -32768 || value > 32767)
      throw new IndexOutOfBoundsException("SynthDef too large to be written in format 1")
    dos.writeShort(value)
  }
  private final val v2Wr  = { (dos: DataOutputStream, value: Int) => dos.writeInt  (value) }

  private[this] final class WireInfo(var index: Int = -1, var numConsumers: Int = 0)

  /** Calculates the number of wire-buffers needed on the server
    * to represent a given ugen-graph.
    *
    * @param in the graph to inspect
    * @return the number of wire buffers required to load the graph in the server
    */
  def calcWireBuffers(in: UGenGraph): Int = {
    val u = in.ugens
    val outputMap =  new Array[Array[WireInfo]](u.size)
    var ui = 0
    u.foreach { indexed =>
      val r = indexed.ugen.outputRates
      val arr = new Array[WireInfo](r.size)
      var ri = 0
      r.foreach {
        case `audio` =>
          val inf = new WireInfo()
          arr(ri) = inf
          ri += 1
        case _ =>
          ri += 1
      }
      outputMap(ui) = arr
      ui += 1
    }

    u.foreach { indexed =>
      indexed.inputSpecs.foreach {
        case (ugenIdx, outIdx) if ugenIdx >= 0 =>
          val wireInfo = outputMap(ugenIdx)(outIdx)
          if (wireInfo != null) wireInfo.numConsumers += 1
        case _ =>
      }
    }

    val bufColor = new impl.WireBufAllocator()

    (u.iterator zip outputMap.iterator).foreach { case (indexed, outputs) =>
      // we never release any input buffers of demand-rate ugens
      if (indexed.ugen.rate != demand) {
        // release inputs
        indexed.inputSpecs.reverseIterator.foreach {
          case (ugenIdx, outIdx) if ugenIdx >= 0 =>
            val wireInfo = outputMap(ugenIdx)(outIdx)
            if (wireInfo != null) {
              bufColor.release(wireInfo.index)
            }
          case _ =>
        }
      }
      // alloc outputs
      outputs.foreach {
        case null     =>
        case wireInfo => wireInfo.index = bufColor.alloc(wireInfo.numConsumers)
      }
    }
    bufColor.size()
  }

  def read(dis: DataInputStream, version: Int): UGenGraph = {
    val readNum = if (version == 1) v1Rd else if (version == 2) v2Rd
                  else throw new IllegalArgumentException(s"SynthDef format version $version not supported")

    // ---- constants ----
    val numConstants  = readNum(dis)
    val constants     = Vector.fill(numConstants)(dis.readFloat())

    // ---- controls ----
    val numCtlValues  = readNum(dis)
    val controlValues = Vector.fill(numCtlValues)(dis.readFloat())

    val numCtlNames   = readNum(dis)
    val controlNames  = Vector.fill(numCtlNames) {
      val name  = readPascalString(dis)
      val index = readNum(dis)
      (name, index)
    }

    val numUGens      = readNum(dis)
    val ugens         = new Array[IndexedUGen](numUGens)

    var idx = 0
    while (idx < numUGens) {
      val name          = readPascalString(dis)
      val rateId        = dis.readUnsignedByte()
      val rate          = Rate(rateId)
      val numInputs     = readNum(dis)
      val numOutputs    = readNum(dis)
      val specialIndex  = dis.readUnsignedShort()

      val inputSpecs    = Vector.fill(numInputs) {
        val _1 = readNum(dis)
        val _2 = readNum(dis)
        (_1, _2)
      }

      val outputRates   = Vector.fill(numOutputs) {
        val rateId = dis.readUnsignedByte()
        Rate(rateId)
      }

      val ugen = new RawUGenImpl(name = name, rate = rate, numInputs = numInputs, outputRates = outputRates,
                                 specialIndex = specialIndex)

      ugens(idx) = new IndexedUGen(ugen, inputSpecs)
      idx += 1
    }

    val numVariants = dis.readUnsignedShort()
    // variants are not supported, we just skip over them
    if (numVariants > 0) for (_ <- 0 until numVariants) {
      readPascalString(dis) // the name of the variant
      for (_ <- 0 until numCtlValues) {
        dis.readFloat() // variant initial parameter values
      }
    }

    UGenGraph(constants, controlValues, controlNames, ugens.toVector)
  }

  @inline private[this] def readPascalString(dis: DataInputStream): String = {
    val len   = dis.readUnsignedByte()
    val arr   = new Array[Byte](len)
    dis.read(arr)
    new String(arr)
  }

  trait BuilderFactory {
    def build(graph: SynthGraph): UGenGraph
  }

  trait Builder {
    def addUGen    (ugen: UGen): Unit
    def prependUGen(ugen: UGen): Unit

    def addControl(values: Vec[Float], name: Option[String]): Int

    def visit[U](ref: AnyRef, init: => U): U
  }

  /** Installs a custom ugen graph builder on the current thread,
    * during the invocation of a closure. This method is typically
    * called from other libraries which wish to provide a graph
    * builder other than the default.
    *
    * When the method returns, the previous graph builder has automatically
    * been restored. During the execution of the `body`, calling
    * `UGenGraph.builder` will return the given `builder` argument.
    *
    * @param builder    the builder to install on the current thread
    * @param body       the body which is executed with the builder found through `UGenGraph.builder`
    * @tparam A         the result type of the body
    * @return           the result of executing the body
    */
  def use[A](builder: Builder)(body: => A): A = {
    val old = builders.get()
    builders.set(builder)
    try {
      body
    } finally {
      builders.set(old)
    }
  }

  // ---- rich ugen ----

  /** A UGen representation indexed inside the UGen graph.
    *
    * @param ugen         the ugen
    * @param inputSpecs   the ugen's inputs indexed as pairs (ugen-index, output-index)
    */
  final class IndexedUGen(val ugen: RawUGen, val inputSpecs: ISeq[(Int, Int)]) {
    override def toString = s"IndexedUGen($ugen, ${inputSpecs.mkString("[", " ", "]")})"
  }

  // ---- graph builder ----

  private final val builders = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  /** The current, thread local ugen graph builder instance. When called
    * outside of an explicit building process, a dummy object will be returned
    * which ignores any calls for adding ugens, but will throw an exception
    * when trying to actually expand any graph element.
    */
  def builder: Builder = builders.get

  private object BuilderDummy extends Builder {
    def build: UGenGraph = outOfContext

    def addControl(values: Vec[Float], name: Option[String]): Int = 0

    def addUGen    (ugen: UGen): Unit = ()
    def prependUGen(ugen: UGen): Unit = ()

    def visit[U](ref: AnyRef, init: => U): U = outOfContext

    private def outOfContext: Nothing = sys.error("Out of context")
  }
}

final case class UGenGraph(constants: Vec[Float], controlValues: Vec[Float],
                           controlNames: Vec[(String, Int)], ugens: Vec[UGenGraph.IndexedUGen]) {
  //   override lazy val hashCode = ... TODO: figure out how case class calculates it...

  def write(dos: DataOutputStream, version: Int): Unit = {
    val writeNum = if (version == 1) UGenGraph.v1Wr else if (version == 2) UGenGraph.v2Wr
                   else throw new IllegalArgumentException(s"SynthDef format version $version not supported")

    // ---- constants ----
    writeNum(dos, constants.size)
    constants.foreach(dos.writeFloat)

    // ---- controls ----
    writeNum(dos, controlValues.size)
    controlValues.foreach(dos.writeFloat)

    writeNum(dos, controlNames.size)
    controlNames.foreach { name =>
      writePascalString(dos, name._1)
      writeNum(dos, name._2)
    }

    writeNum(dos, ugens.size)
    ugens.foreach { ru =>
      val ugen = ru.ugen
      writePascalString(dos, ugen.name)

      dos.writeByte (ugen.rate.id)
      writeNum(dos, ugen.numInputs)
      writeNum(dos, ugen.numOutputs)
      dos.writeShort(ugen.specialIndex)

      ru.inputSpecs.foreach { spec =>
        writeNum(dos, spec._1) // index of unit generator or -1 for a constant
        writeNum(dos, spec._2) // index of constant or index of output
      }

      ugen.outputRates.foreach(r => dos.writeByte(r.id))
    }

    dos.writeShort(0) // variants not supported
  }

  private[this] val charset = Charset.forName("UTF-8")

  private[synth] def writePascalString(dos: DataOutputStream, str: String): Unit = {
    val bytes = str.getBytes(charset)
    val len = bytes.length
    if (len > 255) throw new IllegalArgumentException(s"String '$str' is too long (> 255 bytes)")
    dos.writeByte(len)
    dos.write(bytes)
  }
}