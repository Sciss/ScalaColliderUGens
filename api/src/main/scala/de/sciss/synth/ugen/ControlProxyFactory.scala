/*
 *  ControlProxyFactory.scala
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

import scala.collection.{Seq => SSeq}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object ControlValues {
  // implicit def fromInt      (x :     Int    ): ControlValues = ControlValues(Vector(x.toFloat))
  implicit def fromFloat    (x :      Float  ): ControlValues = ControlValues(Vector(x))
  implicit def fromDouble   (x :      Double ): ControlValues = ControlValues(Vector(x.toFloat))
  implicit def fromIntSeq   (xs: SSeq[Int   ]): ControlValues = {
    val vec: Vec[Float] = xs match {
      case xsv: Vec[Int]  => xsv        .map(_.toFloat)
      case _              => xs.iterator.map(_.toFloat).toIndexedSeq
    }
    ControlValues(vec)
  }
  implicit def fromFloatSeq (xs: SSeq[Float ]): ControlValues = ControlValues(xs.toIndexedSeq)
  implicit def fromDoubleSeq(xs: SSeq[Double]): ControlValues = {
    val vec: Vec[Float] = xs match {
      case xsv: Vec[Double] => xsv        .map(_.toFloat)
      case _                => xs.iterator.map(_.toFloat).toIndexedSeq
    }
    ControlValues(vec)
  }
  private[ugen] val singleZero = ControlValues(Vector(0f))
}
final case class ControlValues(seq: Vec[Float])

final class ControlProxyFactory(val `this`: String) extends AnyVal { me =>
  import me.{`this` => name}

  def ir: ControlProxy = ir(ControlValues.singleZero)
  def ir(values: ControlValues): ControlProxy      = ControlProxy(scalar,  values.seq, Some(name))

  def kr: ControlProxy = kr(ControlValues.singleZero)
  def kr(values: ControlValues): ControlProxy      = ControlProxy(control, values.seq, Some(name))

  def tr: TrigControlProxy = tr(ControlValues.singleZero)
  def tr(values: ControlValues): TrigControlProxy  = TrigControlProxy     (values.seq, Some(name))

  def ar: AudioControlProxy = ar(ControlValues.singleZero)
  def ar(values: ControlValues): AudioControlProxy = AudioControlProxy    (values.seq, Some(name))
}

trait ControlFactoryLike {
  def build(b: UGenGraph.Builder, proxies: Vec[ControlProxyLike]): Map[ControlProxyLike, (UGen, Int)] = {
    val sz = proxies.size
    if (sz == 0) return Map.empty

    var numChannels   = 0
    var specialIndex  = -1
    proxies.foreach { p =>
      numChannels += p.values.size
      val i = b.addControl(p.values, p.name)
      if (specialIndex < 0) specialIndex = i
    }

    val ugen = makeUGen(numChannels, specialIndex)

    var offset  = 0
    val resB    = Map.newBuilder[ControlProxyLike, (UGen, Int)]
    resB.sizeHint(sz)
    proxies.foreach { p =>
      val res = p -> ((ugen, offset))
      offset += p.values.size
      resB += res
    }
    resB.result()
  }

  protected def makeUGen(numChannels: Int, specialIndex: Int): UGen
}

trait ControlProxyLike extends GE {
  // ---- constructor ----
  SynthGraph.builder.addControlProxy(this)

  def rate    : Rate
  private[synth] def factory: ControlFactoryLike
  def name    : Option[String]
  def values  : Vec[Float]

  /** Note: this expands to a single ControlUGenOutProxy for numChannels == 1,
    * otherwise to a sequence of proxies wrapped in UGenInGroup. Therefore,
    * {{{
    *    In.ar( "in".kr, 2 )
    * }}}
    * results in an `In` UGen, and does not re-wrap into a UGenInGroup
    * (e.g. behaves like `In.ar( 0, 2 )` and not `In.ar( Seq( 0 ), 2 )` which
    * would mess up successive multi channel expansion.
    *
    * This is kind of a particular way of producing the proper `isWrapped` results.
    */
  final private[synth] def expand: UGenInLike = if (values.size == 1) {
    ControlUGenOutProxy(this, 0)
  } else {
    UGenInGroup(Vector.tabulate(values.size)(i => ControlUGenOutProxy(this, i)))
  }
}