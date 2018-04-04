/*
 *  UGen.scala
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

import de.sciss.synth.ugen.impl.{MultiOutImpl, SingleOutImpl, ZeroOutImpl}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions
import scala.runtime.ScalaRunTime

/** The raw UGen information as it is found in a final `UGenGraph`. */
trait RawUGen {
  def name        : String

  def rate        : Rate

  def numInputs   : Int

  def numOutputs  : Int

  def outputRates : Vec[Rate]

  def specialIndex: Int
}

/** A UGen during graph building process is a more
  * rich thing than `RawUGen`: it implements equality
  * based on `isIndividual` status and may be omitted
  * from the final graph based on `hasSideEffect` status.
  */
sealed trait UGen extends RawUGen with Product {
  // initialize this first, so that debug printing in `addUGen` can use the hash code
  override val hashCode: Int = if (isIndividual) super.hashCode() else ScalaRunTime._hashCode(this)

  override def toString: String = {
    val ins = inputs.mkString("(", ", ", ")")
    s"$name.${rate.methodName}$ins"
  }

  def inputs      : Vec[UGenIn]

  def numInputs: Int = inputs.size

  // the full UGen spec:
  // name, rate, specialIndex, inputs, outputRates
  override final def productPrefix: String = "UGen"
  final def productArity: Int = 5

  final def productElement(n: Int): Any = (n: @switch) match {
    case 0 => name
    case 1 => rate
    case 2 => specialIndex
    case 3 => inputs
    case 4 => outputRates
    case _ => throw new java.lang.IndexOutOfBoundsException(n.toString)
  }

  final def canEqual(x: Any): Boolean = x.isInstanceOf[UGen]

  override def equals(x: Any): Boolean = (this eq x.asInstanceOf[AnyRef]) || (!isIndividual && (x match {
    case u: UGen =>
      u.hashCode == hashCode &&
      u.name == name && u.rate == rate && u.specialIndex == specialIndex && u.inputs == inputs &&
        u.outputRates == outputRates && u.canEqual(this)
    case _ => false
  }))

  def isIndividual : Boolean
  def hasSideEffect: Boolean
}

object UGen {
  object SingleOut {
    def apply(name: String, rate: Rate, inputs: Vec[UGenIn], isIndividual: Boolean = false,
              hasSideEffect: Boolean = false, specialIndex: Int = 0): SingleOut = {
      val res = new SingleOutImpl(name, rate, inputs, isIndividual = isIndividual, hasSideEffect = hasSideEffect,
                                  specialIndex = specialIndex)
      UGenGraph.builder.addUGen(res)
      res
    }
  }
  /** A SingleOutUGen is a UGen which has exactly one output, and
    * hence can directly function as input to another UGen without expansion.
    */
  trait SingleOut extends ugen.UGenProxy with UGen {
    final def numOutputs = 1
    final def outputRates: Vec[Rate] = rate.toIndexedSeq
    final def outputIndex = 0
    final def source: UGen = this
  }

  object ZeroOut {
    def apply(name: String, rate: Rate, inputs: Vec[UGenIn], isIndividual: Boolean = false,
              specialIndex: Int = 0): ZeroOut = {
      val res = new ZeroOutImpl(name, rate, inputs, isIndividual = isIndividual, specialIndex = specialIndex)
      UGenGraph.builder.addUGen(res)
      res
    }
  }
  trait ZeroOut extends UGen {
    final def numOutputs = 0
    final def outputRates: Vec[Rate] = Vector.empty
    final def hasSideEffect = true  // implied by having no outputs
  }

  object MultiOut {
    def apply(name: String, rate: Rate, outputRates: Vec[Rate], inputs: Vec[UGenIn],
              isIndividual: Boolean = false, hasSideEffect: Boolean = false, specialIndex: Int = 0): MultiOut = {
      val res = new MultiOutImpl(name, rate, outputRates, inputs, isIndividual = isIndividual,
        hasSideEffect = hasSideEffect, specialIndex = specialIndex)
      UGenGraph.builder.addUGen(res)
      res
    }
  }
  /** A class for UGens with multiple outputs. */
  trait MultiOut extends ugen.UGenInGroup with UGen {

    final def numOutputs: Int = outputRates.size

    final def unwrap(i: Int): UGenInLike = ugen.UGenOutProxy(this, i % numOutputs)

    def outputs: Vec[UGenIn] = Vector.tabulate(numOutputs)(ch => ugen.UGenOutProxy(this, ch))

    /*
         This is important: Imagine for example `Out.ar( PanAz.ar( 4, In.ar( 0, 1 ), pos ))`.
         If `isWrapped` would report `true` here, the result would be a re-wrapping into four
         `Out` UGens. Obviously we do not want this.

         This corresponds with the following check in method `MultiOutUGen: initOutputs` in sclang:
         `^if( numChannels == 1, { channels.at( 0 )}, channels )`!
       */
    //      private[synth] final def isWrapped = numOutputs != 1
    private[synth] final def unbubble: UGenInLike = if (numOutputs == 1) outputs(0) else this
  }
}

object UGenInLike {
  implicit def expand(ge: GE): UGenInLike = ge.expand
}
/** A super-trait that contains `UGenIn` and `UGenInGroup`. */
sealed trait UGenInLike extends GE {
  private[synth] def outputs: Vec[UGenInLike]
  private[synth] def unbubble: UGenInLike

  /** Returns the UGenInLike element of index i
    * regarding the ungrouped representation. Note
    * that for efficiency reasons this method will
    * automatically wrap the index around numElements!
    */
  private[synth] def unwrap(i: Int): UGenInLike
  private[synth] def flatOutputs: Vec[UGenIn]

  // ---- GE ----
  final private[synth] def expand: UGenInLike = this
}

/** An element that can be used as an input to a UGen.
  * This is after multi-channel-expansion, hence implementing
  * classes are SingleOutUGen, UGenOutProxy, ControlOutProxy, and Constant.
  */
sealed trait UGenIn extends UGenInLike {
  def rate: Rate

  private[synth] def outputs: Vec[UGenIn] = Vector(this)
  private[synth] final def unwrap(i: Int): UGenInLike = this

  // don't bother about the index
  private[synth] final def flatOutputs: Vec[UGenIn] = Vector(this)
  private[synth] final def unbubble   : UGenInLike  = this
}

package ugen {
  object UGenInGroup {
    private final val emptyVal = Apply(Vector.empty)
    def empty: UGenInGroup = emptyVal
    def apply(xs: Vec[UGenInLike]): UGenInGroup = Apply(xs)

    private final case class Apply(outputs: Vec[UGenInLike]) extends UGenInGroup {
      override def productPrefix = "UGenInGroup"

      private[synth] def numOutputs: Int = outputs.size
      private[synth] def unwrap(i: Int): UGenInLike = outputs(i % outputs.size)
      private[synth] def unbubble: UGenInLike = this

      override def toString: String = outputs.mkString("UGenInGroup(", ",", ")")

      // ---- GE ----
      def rate: MaybeRate = MaybeRate.reduce(outputs.map(_.rate): _*)
    }
  }
  /** A trait that can be either a group of `UGenInLike` or a `UGen.MultiOut` */
  sealed trait UGenInGroup extends UGenInLike {
    private[synth] def outputs: Vec[UGenInLike]
    private[synth] def numOutputs: Int
    private[synth] final def flatOutputs: Vec[UGenIn] = outputs.flatMap(_.flatOutputs)
  }

  sealed trait UGenProxy extends UGenIn {
    def source: UGen
    def outputIndex: Int
  }

  object Constant {
    final val C0  = new Constant(0)
    final val C1  = new Constant(1)
    final val Cm1 = new Constant(-1)
  }
  /** A scalar constant used as an input to a UGen.
    * These constants are stored in a separate table of
    * the synth graph.
    */
  final case class Constant(value: Float) extends UGenIn with ScalarRated {
    override def toString: String = value.toString
  }

  /** A ControlOutProxy is similar to a UGenOutProxy in that it denotes
    * an output channel of a control UGen. However it refers to a control-proxy
    * instead of a real control ugen, since the proxies are synthesized into
    * actual ugens only at the end of a synth graph creation, in order to
    * clump several controls together. ControlOutProxy instance are typically
    * returned from the ControlProxyFactory class, that is, using the package
    * implicits, from calls such as "myControl".kr.
    */
  final case class ControlUGenOutProxy(source: ControlProxyLike, outputIndex: Int /*, rate: Rate */)
    extends UGenIn {

    def rate: Rate = source.rate
    override def toString = s"$source.\\($outputIndex)"
  }

  /** A UGenOutProxy refers to a particular output of a multi-channel UGen.
    * A sequence of these form the representation of a multi-channel-expanded
    * UGen.
    */
  final case class UGenOutProxy(source: UGen.MultiOut, outputIndex: Int /*, rate: Rate */)
    extends UGenIn with UGenProxy {

    override def toString: String =
      if (source.numOutputs == 1) source.toString else s"$source.\\($outputIndex)"

    def rate: Rate = source.outputRates(outputIndex)
  }
}