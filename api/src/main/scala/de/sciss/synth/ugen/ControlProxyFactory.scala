/*
 *  ControlProxyFactory.scala
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

import collection.breakOut
import collection.immutable.{IndexedSeq => Vec}
import language.implicitConversions

object ControlValues {
  // implicit def fromInt      (x :     Int    ): ControlValues = ControlValues(Vector(x.toFloat))
  implicit def fromFloat    (x :     Float  ): ControlValues = ControlValues(Vector(x))
  implicit def fromDouble   (x :     Double ): ControlValues = ControlValues(Vector(x.toFloat))
  implicit def fromIntSeq   (xs: Seq[Int   ]): ControlValues = ControlValues(xs.map(_.toFloat)(breakOut))
  implicit def fromFloatSeq (xs: Seq[Float ]): ControlValues = ControlValues(xs.toIndexedSeq)
  implicit def fromDoubleSeq(xs: Seq[Double]): ControlValues = ControlValues(xs.map(_.toFloat)(breakOut))
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
    var numChannels = 0
    val specialIndex = proxies.map { p =>
      numChannels += p.values.size
      b.addControl(p.values, p.name)
    } .head
    val ugen = makeUGen(numChannels, specialIndex)
    var offset = 0
    proxies.map { p =>
      val res = p -> ((ugen, offset))
      offset += p.values.size
      res
    } (breakOut)
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