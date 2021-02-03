/*
 *  UGenSource.scala
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

import de.sciss.serial
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.ugen.{Constant, ControlProxyLike}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.runtime.ScalaRunTime

object UGenSource {
  abstract class ZeroOut   extends UGenSource[Unit]
  abstract class SingleOut extends SomeOut
  abstract class MultiOut  extends SomeOut

  sealed abstract class SomeOut extends UGenSource[UGenInLike] with GE.Lazy

  // ---- utilities ----

  final val inf = Float.PositiveInfinity

  type Vec[+A] = scala.collection.immutable.IndexedSeq[A]
  val  Vec     = scala.collection.immutable.IndexedSeq

  def stringArg(s: String): Vec[UGenIn] = {
    val bs: Array[Byte] = s.getBytes
    val resB = Vec.newBuilder[UGenIn]
    resB.sizeHint(bs.length + 1)
    resB += Constant(bs.length)
    bs.foreach { b =>
      resB += Constant(b)
    }
    resB.result()
  }

  def unwrap(source: UGenSource.SomeOut, args: Vec[UGenInLike]): UGenInLike = {
    var uIns    = Vector.empty: Vec[UGenIn]
    var uInsOk  = true
    var exp     = 0
    args.foreach(_.unbubble match {
      case u: UGenIn => if (uInsOk) uIns :+= u
      case g: ugen.UGenInGroup =>
        exp     = math.max(exp, g.numOutputs)
        uInsOk  = false // don't bother adding further UGenIns to uIns
    })
    if (uInsOk) {
      // aka uIns.size == args.size
      source.makeUGen(uIns)
    } else {
      // rewrap(args, exp)
      ugen.UGenInGroup(Vector.tabulate(exp)(i => unwrap(source, args.map(_.unwrap(i)))))
    }
  }

  def unwrap(source: UGenSource.ZeroOut, args: Vec[UGenInLike]): Unit = {
    var uIns    = Vector.empty: Vec[UGenIn]
    var uInsOk  = true
    var exp     = 0
    args.foreach(_.unbubble match {
      case u: UGenIn => if (uInsOk) uIns :+= u
      case g: ugen.UGenInGroup =>
        exp     = math.max(exp, g.numOutputs)
        uInsOk  = false // don't bother adding further UGenIns to uIns
    })
    if (uInsOk) {
      // aka uIns.size == args.size
      source.makeUGen(uIns)
    } else {
      // rewrap(args, exp)
      var i = 0
      while (i < exp) {
        unwrap(source, args.map(_.unwrap(i)))
        i += 1
      }
    }
  }

  /** Simple forwarder to `in.expand` that can be used to access
    * the otherwise package-private method.
    */
  def expand(in: GE): UGenInLike = in.expand

  /** Simple forwarder to `in.outputs` that can be used to access
    * the otherwise package-private method.
    */
  def outputs(in: UGenInLike): Vec[UGenInLike] = in.outputs

  /** Simple forwarder to `in.flatOutputs` that can be used to access
    * the otherwise package-private method.
    */
  def flatOutputs(in: UGenInLike): Vec[UGenIn] = in.flatOutputs

  /** Simple forwarder to `in.unbubble` that can be used to access
    * the otherwise package-private method.
    */
  def unbubble(in: UGenInLike): UGenInLike = in.unbubble

  /** Simple forwarder to `in.unwrap` that can be used to access
    * the otherwise package-private method.
    */
  def unwrapAt(in: UGenInLike, index: Int): UGenInLike = in.unwrap(index)

  // if the input at index `idx` has a different rate than `target`, update the
  // that input by wrapping it inside a conversion UGen
  def matchRate(ins: Vec[UGenIn], idx: Int, target: Rate): Vec[UGenIn] = {
    val in = ins(idx)
    if (in.rate == target) ins else {
      if (target == audio) {
        val ugenName  = if (in.rate == scalar) "DC" else "K2A"
        val upd       = UGen.SingleOut(ugenName, audio, Vector(in))
        ins.updated(idx, upd)
      } else {  // target == control | scalar
        if (in.rate == audio) {
          val ugenName  = if (target == scalar) "DC" else "A2K"
          val upd       = UGen.SingleOut(ugenName, control, Vector(in))
          ins.updated(idx, upd)
        } else ins  // can use `scalar` where `control` is required
      }
    }
  }

  // repeats `matchRate` for all arguments beginning at `idx` to the end of `ins`
  @tailrec def matchRateFrom(ins: Vec[UGenIn], idx: Int, target: Rate): Vec[UGenIn] =
  if (idx == ins.size) ins else {
    val ins1 = matchRate(ins, idx, target)
    matchRateFrom(ins1, idx + 1, target)
  }

  // same as matchRate but assuming that the input is a trigger signal
  def matchRateT(ins: Vec[UGenIn], idx: Int, target: Rate): Vec[UGenIn] = {
    val in = ins(idx)
    if (in.rate == target) ins else {
      if (target == audio) {
        val ugenName  = if (in.rate == scalar) "DC" else "T2A"
        val upd       = UGen.SingleOut(ugenName, audio, Vector(in))
        ins.updated(idx, upd)
      } else {  // target == control | scalar
        if (in.rate == audio) {
          val ugenName  = if (target == scalar) "DC" else "T2K"
          val upd       = UGen.SingleOut(ugenName, control, Vector(in))
          ins.updated(idx, upd)
        } else ins  // can use `scalar` where `control` is required
      }
    }
  }

  // ---- serialization ----

//  type ProductReader[+A] = serial.ProductReader[RefMapIn, A]

  trait ProductReader[+A] {
    def read(in: RefMapIn, key: String, arity: Int): A
  }

  private val mapRead = mutable.Map.empty[String, ProductReader[Product]]

  final class RefMapIn(in0: DataInput) extends serial.RefMapIn[RefMapIn](in0) {
    type Const  = Constant
    type R      = MaybeRate
    type Y      = SynthGraph

    override protected def readProductWithKey(key: String, arity: Int): Product = {
      val r = mapRead.getOrElse(key, throw new NoSuchElementException(s"Unknown element '$key'"))
      r.read(this, key, arity)
    }

    override protected def readIdentifiedConst(): Constant = {
      val value = in.readFloat()
      Constant(value)
    }

    override def readIdentifiedY(): SynthGraph = readIdentifiedGraph()

    def readGraph(): SynthGraph = {
      val cookie = in0.readByte().toChar
      if (cookie != 'Y') unexpectedCookie(cookie, 'Y')
      readIdentifiedGraph()
    }

    def readIdentifiedGraph(): SynthGraph = {
      val sources   = readVec(readProductT[Lazy]())
      val controls  = readSet(readProductT[ControlProxyLike]())
      SynthGraph(sources, controls)
    }

    def readGE(): GE =
      readProduct().asInstanceOf[GE]

    def readGEDone(): GE with HasDoneFlag =
      readProduct().asInstanceOf[GE with HasDoneFlag]

    override protected def readIdentifiedR(): MaybeRate = {
      val id = in.readByte().toInt
      MaybeRate(id)
    }

    def readRate(): Rate = {
      val cookie = in.readByte().toChar
      if (cookie != 'R') sys.error(s"Unexpected cookie '$cookie' is not 'R'")
      val id = in.readByte().toInt
      Rate(id)
    }

    def readMaybeRate(): MaybeRate = {
      val cookie = in.readByte().toChar
      if (cookie != 'R') sys.error(s"Unexpected cookie '$cookie' is not 'R'")
      readIdentifiedR()
    }
  }

  final val DefaultPackage = "de.sciss.synth.ugen"

  final class RefMapOut(out0: DataOutput) extends serial.RefMapOut(out0) {
    override protected def isDefaultPackage(pck: String): Boolean =
      pck == DefaultPackage

    override def writeElem(e: Any): Unit = e match {
      case c: Constant =>
        out.writeByte('C')
        out.writeFloat(c.value)
      case r: MaybeRate =>
        out.writeByte('R')
        out.writeByte(r.id)
      case y: SynthGraph =>
        out.writeByte('Y')
        writeIdentifiedGraph(y)
      case _ => super.writeElem(e)
    }

    def writeIdentifiedGraph(v: SynthGraph): Unit = {
      writeVec(v.sources        , writeProduct)
      writeSet(v.controlProxies , writeProduct)
    }
  }

  def addProductReader(prefix: String, r: ProductReader[Product]): Unit = mapRead.synchronized {
    val old = mapRead.put(prefix, r)
    require (old.isEmpty, {
      mapRead.put(prefix, old.get)
      sys.error(s"Prefix '$prefix' was already registered")
    })
  }

  def addProductReaders(xs: Iterable[(String, ProductReader[Product])]): Unit = mapRead.synchronized {
    mapRead ++= xs
    ()
  }

  /** Derives the `productPrefix` served by the reader by the reader's class name itself.  */
  def addProductReaderSq(xs: Iterable[ProductReader[Product]]): Unit = {
    val m = mapRead
    m.synchronized {
      xs.foreach { value =>
        val cn    = value.getClass.getName
        val nm    = cn.length - 1
        val isObj = cn.charAt(nm) == '$'
        val i     = if (cn.startsWith(DefaultPackage)) DefaultPackage.length + 1 else 0
        val key   = if (isObj) cn.substring(i, nm) else cn.substring(i)
        m += ((key, value))
      }
    }
  }
}

sealed abstract class UGenSource[U] extends Lazy.Expander[U] {
  protected def makeUGen(args: Vec[UGenIn]): U

  final def name: String = productPrefix

  override lazy val hashCode: Int = ScalaRunTime._hashCode(this)
}