/*
 *  GE.scala
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

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.{Seq => SSeq}

/** The UGen graph is constructed from interconnecting graph elements (GE).
  * Graph elements can be decomposed into a sequence of UGenIn objects.
  * Graph elements are ordinary UGens, UGen proxies, Control proxies,
  * Constants, and collections of UGen inputs which result from
  * multichannel expansion (UGenInSeq).
  */
object GE {
  import ugen.Constant

  import language.implicitConversions

  implicit def const(i: Int   ): Constant = new Constant(i)
  implicit def const(f: Float ): Constant = new Constant(f)
  implicit def const(d: Double): Constant = new Constant(d.toFloat)

  // XXX don't we expect Multi[GE[R]] ?
  implicit def fromSeq(xs: SSeq[GE]): GE = xs match {
    case SSeq(x)  => x
    case _        => ugen.GESeq(xs.toIndexedSeq)
  }

  implicit def fromIntSeq(xs: SSeq[Int]): GE = xs match {
    case SSeq(x) => Constant(x)
    case _ =>
      val vec: Vec[GE] = xs match {
        case xsv: Vec[Int]  => xsv        .map(i => Constant(i.toFloat))
        case _              => xs.iterator.map(i => Constant(i.toFloat)).toIndexedSeq
      }
      ugen.GESeq(vec)
  }

  implicit def fromFloatSeq(xs: SSeq[Float]): GE = xs match {
    case SSeq(x) => Constant(x)
    case _ =>
      val vec: Vec[GE] = xs match {
        case xsv: Vec[Float]  => xsv        .map(i => Constant(i))
        case _                => xs.iterator.map(i => Constant(i)).toIndexedSeq
      }
      ugen.GESeq(vec)
  }

  implicit def fromDoubleSeq(xs: SSeq[Double]): GE = xs match {
    case SSeq(x) => Constant(x.toFloat)
    case _ =>
      val vec: Vec[GE] = xs match {
        case xsv: Vec[Double] => xsv        .map(i => Constant(i.toFloat))
        case _                => xs.iterator.map(i => Constant(i.toFloat)).toIndexedSeq
      }
      ugen.GESeq(vec)
  }

  def fromUGenIns(xs: SSeq[UGenIn]): GE = ugen.UGenInSeq(xs.toIndexedSeq)

//  def fromString(s: String): GE = ugen.stringArg(s)

  /** Simply a trait composed of `Lazy.Expander[UGenInLike]` and `GE`. */
  trait Lazy extends Lazy.Expander[UGenInLike] with GE
}

/** The main trait used in synthesis graph, a graph element, abbreviated as `GE`.
  *
  * Graph elements are characterized by having a calculation rate (possibly unknown),
  * and they embody future UGens, which are created by invoking the `expand` method.
  * For each ugen in SuperCollider, there is a corresponding graph element defined
  * in the `ugen` package, and these elements take again graph elements as arguments.
  * Multi-channel expansion is thus deferred to the transition from `SynthGraph` to `UGenGraph`.
  *
  * Currently, also a lot of unary and binary operations are directly defined on the `GE` trait,
  * although they might go into a separate `GEOps` implicit class in future versions.
  *
  * @see [[de.sciss.synth.SynthGraph]]
  */
trait GE extends Product {
  def rate: MaybeRate
  private[synth] def expand: UGenInLike
  //  private[synth] def productPrefix: String
}

package ugen {
  object GESeq {
    def apply(elems: GE*): GESeq = new GESeq(elems.toIndexedSeq)
  }
  final case class GESeq(elems: Vec[GE]) extends GE {
    def numOutputs: Int           = elems.size
    def expand    : UGenInLike    = UGenInGroup(elems.map(_.expand))
    def rate      : MaybeRate     = MaybeRate.reduce(elems.map(_.rate): _*)

    override def toString: String = elems.mkString("GESeq(", ",", ")")
  }

  private[synth] final case class UGenInSeq(elems: Vec[UGenIn]) extends GE {
    def numOutputs: Int           = elems.size
    def expand    : UGenInLike    = UGenInGroup(elems)
    def rate      : MaybeRate     = MaybeRate.reduce(elems.map(_.rate): _*)

    override def toString: String = elems.mkString("UGenInSeq(", ",", ")")
  }
}