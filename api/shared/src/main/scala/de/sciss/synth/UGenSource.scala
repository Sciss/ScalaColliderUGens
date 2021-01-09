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

import de.sciss.serial.DataInput
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.ugen.Constant

import scala.annotation.{switch, tailrec}
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

  trait ProductReader[+A] {
    // should we call this `readIdentified`?
    def read(in: RefMapIn, arity: Int): A
  }

  private val mapRead = mutable.Map.empty[String, ProductReader[Product]]

  final class RefMapIn(_in: DataInput) {
    private[this] val map   = mutable.Map.empty[Int, Product]
    private[this] var count = 0

    def in: DataInput = _in

    def readProduct(): Product = {
      val cookie = _in.readByte().toChar
      (cookie: @switch) match {
        case 'C' =>
          val value = _in.readFloat()
          Constant(value)

        case 'P' =>
          val prefix  = _in.readUTF()
          val r       = mapRead.getOrElse(prefix, throw new NoSuchElementException(s"Unknown element '$prefix'"))
          val arity   = in.readShort().toInt
          val res     = r.read(this, arity)
          val id      = count
          map    += ((id, res))
          count   = id + 1
          res

        case '<' =>
          val id = _in.readInt()
          map(id)

        case _ =>
          sys.error(s"Unexpected cookie '$cookie' is not 'P'")
      }
    }

    def readGE(): GE =
      readProduct().asInstanceOf[GE]

    def readGEDone(): GE with HasDoneFlag =
      readProduct().asInstanceOf[GE with HasDoneFlag]

    private def readVec[A](elem: => A): Vec[A] = {
      val cookie = _in.readByte()
      if (cookie != 'X') sys.error(s"Unexpected cookie '$cookie' is not 'X'")
      val size = in.readInt()
      Vector.fill(size)(elem)
    }

    def readGEVec(): Vec[GE] = readVec(readGE())

    def readInt(): Int = {
      val cookie = _in.readByte()
      if (cookie != 'I') sys.error(s"Unexpected cookie '$cookie' is not 'I'")
      _in.readInt()
    }

    def readIntVec(): Vec[Int] = readVec(readInt())

    def readString(): String = {
      val cookie = _in.readByte()
      if (cookie != 'S') sys.error(s"Unexpected cookie '$cookie' is not 'S'")
      _in.readUTF()
    }

    def readStringOption(): Option[String] = {
      val cookie = _in.readByte()
      if (cookie != 'O') sys.error(s"Unexpected cookie '$cookie' is not 'O'")
      val defined = in.readBoolean()
      if (defined) Some(readString()) else None
    }

    def readFloat(): Float = {
      val cookie = _in.readByte()
      if (cookie != 'F') sys.error(s"Unexpected cookie '$cookie' is not 'F'")
      in.readFloat()
    }

    def readDouble(): Double = {
      val cookie = _in.readByte()
      if (cookie != 'D') sys.error(s"Unexpected cookie '$cookie' is not 'D'")
      in.readDouble()
    }

    def readFloatVec(): Vec[Float] = readVec(readFloat())

    def readRate(): Rate = {
      val cookie = _in.readByte().toChar
      if (cookie != 'R') sys.error(s"Unexpected cookie '$cookie' is not 'R'")
      val id = _in.readByte().toInt
      Rate(id)
    }

    def readMaybeRate(): MaybeRate = {
      val cookie = in.readByte().toChar
      if (cookie != 'R') sys.error(s"Unexpected cookie '$cookie' is not 'R'")
      val id = in.readByte().toInt
      MaybeRate(id)
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
}

sealed abstract class UGenSource[U] extends Lazy.Expander[U] {
  protected def makeUGen(args: Vec[UGenIn]): U

  final def name: String = productPrefix

  override lazy val hashCode: Int = ScalaRunTime._hashCode(this)
}