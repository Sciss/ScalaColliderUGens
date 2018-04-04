/*
 *  UGenSource.scala
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

import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.ugen.Constant

import scala.annotation.tailrec
import scala.collection.breakOut
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
    val bs = s.getBytes
    Constant(bs.length) +: (bs.map(Constant(_))(breakOut): Vec[UGenIn])
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
}

sealed abstract class UGenSource[U] extends Lazy.Expander[U] {
  protected def makeUGen(args: Vec[UGenIn]): U

  final def name: String = productPrefix

  override lazy val hashCode: Int = ScalaRunTime._hashCode(this)
}