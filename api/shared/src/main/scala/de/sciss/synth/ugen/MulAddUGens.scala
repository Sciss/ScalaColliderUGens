/*
 *  MulAddUGens.scala
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

import de.sciss.synth.UGenSource._
import de.sciss.synth.ugen.Constant.{C0, C1, Cm1}

object MulAdd extends ProductType[MulAdd] {
  final val typeId = 282

  override def read(in: RefMapIn, key: String, arity: Int): MulAdd = {
    require (arity == 3)
    val _in   = in.readGE()
    val _mul  = in.readGE()
    val _add  = in.readGE()
    new MulAdd(_in, _mul, _add)
  }
}
/** A UGen that multiplies an input with another signal
  * and then adds a third signal. This can be used to translate
  * an element into a different value range.
  *
  * Usually the graph element operator `mulAdd` is used
  * instead of explicitly writing `MulAdd`.
  *
  * {{{
  * MulAdd(in, mul, add) == in.mulAdd(mul, add) == in * mul + add
  * }}}
  *
  * ===Examples===
  *
  * {{{
  * // scale value range
  * play {
  *   // input range -1 to +1,
  *   // output range ((-1 * 100) + 300) = 200
  *   // to           ((+1 * 100) + 300) = 400
  *   val freq = SinOsc.kr(1).mulAdd(100, 300) // or MulAdd(..., 100, 300)
  *   freq.poll(label = "freq")
  *   SinOsc.ar(freq) * 0.1
  * }
  * }}}
  *
  * @param in   the input signal to scale and offset
  * @param mul  the scaling factor, applied first to the input
  * @param add  the offset, added after applying the multiplication
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.Sum3$ Sum3]]
  * @see [[de.sciss.synth.ugen.Sum4$ Sum4]]
  */
final case class MulAdd(in: GE, mul: GE, add: GE)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in.expand, mul.expand, add.expand))

  def rate: MaybeRate = in.rate // XXX TODO - correct?

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    import BinaryOpUGen.{Minus, Plus, Times}
    import UnaryOpUGen.Neg

    val in0  = args(0)
    val mul0 = args(1)
    val add0 = args(2)
    (mul0, add0) match {
      case (C0, _)    => add0
      case (C1, C0)   => in0
      case (C1, _)    => Plus .make1(in0, add0)
      case (Cm1, C0)  => Neg  .make1(in0)
      case (_, C0)    => Times.make1(in0, mul0)
      case (Cm1, _)   => Minus.make1(add0, in0)
      case _              =>
        if (in0.rate == `audio` ||
          (in0.rate == `control` && (mul0.rate == `control` || mul0.rate == `scalar`) &&
            (add0.rate == `control` || add0.rate == `scalar`))) {
          UGen.SingleOut(name, in0.rate, args)
        } else {
          Plus.make1(Times.make1(in0, mul0), add0)
        }
    }
  }

  override def toString = s"$in.madd($mul, $add)"
}

/** A UGen to efficiently add three signals together.
  * Usually used indirectly through `Mix`.
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum4$ Sum4]]
  */
object Sum3 extends ProductType[Sum3] {
  private[ugen] def make1(args: Vec[UGenIn]): UGenIn = {
    val in0i = args(0)
    val in1i = args(1)
    val in2i = args(2)
    import BinaryOpUGen.Plus
    if (in0i == C0) {
      Plus.make1(in1i, in2i)
    } else if (in1i == C0) {
      Plus.make1(in0i, in2i)
    } else if (in2i == C0) {
      Plus.make1(in0i, in1i)
    } else {
      val rate  = in0i.rate max in1i.rate max in2i.rate
      val argsM = if (rate == audio) matchRateFrom(args, 0, audio) else args
      UGen.SingleOut("Sum3", rate, argsM)
    }
  }

  final val typeId = 283

  override def read(in: RefMapIn, key: String, arity: Int): Sum3 = {
    require (arity == 3)
    val _in0 = in.readGE()
    val _in1 = in.readGE()
    val _in2 = in.readGE()
    new Sum3(_in0, _in1, _in2)
  }
}

/** A UGen to efficiently add three signals together.
  * Usually used indirectly through `Mix`.
  *
  * @param in0  first of the three inputs
  * @param in1  second of the three inputs
  * @param in2  third of the three inputs
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum4$ Sum4]]
  */
final case class Sum3(in0: GE, in1: GE, in2: GE) extends UGenSource.SingleOut {
  def rate: MaybeRate = MaybeRate.max_?(in0.rate, in1.rate, in2.rate)

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in0.expand, in1.expand, in2.expand))

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = Sum3.make1(args)
}

/** A UGen to efficiently add four signals together.
  * Usually used indirectly through `Mix`.
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum3$ Sum3]]
  */
object Sum4 extends ProductType[Sum4] {
  private[ugen] def make1(args: Vec[UGenIn]): UGenIn = {
    val in0i = args(0)
    val in1i = args(1)
    val in2i = args(2)
    val in3i = args(3)

    if (in0i == C0) {
      Sum3.make1(Vec(in1i, in2i, in3i))
    } else if (in1i == C0) {
      Sum3.make1(Vec(in0i, in2i, in3i))
    } else if (in2i == C0) {
      Sum3.make1(Vec(in0i, in1i, in3i))
    } else if (in3i == C0) {
      Sum3.make1(Vec(in0i, in1i, in2i))
    } else {
      val rate  = in0i.rate max in1i.rate max in2i.rate max in3i.rate
      val argsM = if (rate == audio) matchRateFrom(args, 0, audio) else args
      UGen.SingleOut("Sum4", rate, argsM)
    }
  }

  final val typeId = 284

  override def read(in: RefMapIn, key: String, arity: Int): Sum4 = {
    require (arity == 4)
    val _in0 = in.readGE()
    val _in1 = in.readGE()
    val _in2 = in.readGE()
    val _in3 = in.readGE()
    new Sum4(_in0, _in1, _in2, _in3)
  }
}
/** A UGen to efficiently add four signals together.
  * Usually used indirectly through `Mix`.
  *
  * @param in0  first of the four inputs
  * @param in1  second of the four inputs
  * @param in2  third of the four inputs
  * @param in3  fourth of the four inputs
  *
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  * @see [[de.sciss.synth.ugen.Sum3$ Sum3]]
  */
final case class Sum4(in0: GE, in1: GE, in2: GE, in3: GE) extends UGenSource.SingleOut {
  def rate: MaybeRate = MaybeRate.max_?(in0.rate, in1.rate, in2.rate, in3.rate)

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in0.expand, in1.expand, in2.expand, in3.expand))

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = Sum4.make1(args)
}
