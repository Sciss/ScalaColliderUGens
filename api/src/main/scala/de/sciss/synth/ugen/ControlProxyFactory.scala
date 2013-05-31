/*
 *  ControlProxyFactory.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}

object ControlProxyFactory {
  private val controlIrFactory = new ControlFactory(scalar )
  private val controlKrFactory = new ControlFactory(control)
}
final class ControlProxyFactory(name: String) {
  import ControlProxyFactory._

  def ir: ControlProxy = ir(IIdxSeq(0f))
  def ir(value: Double, values: Double*): ControlProxy      = ir(IIdxSeq((value.toFloat +: values.map(_.toFloat)): _*))
  def ir(value: Float,  values: Float* ): ControlProxy      = ir(IIdxSeq((value +: values): _*))

  def kr: ControlProxy = kr(IIdxSeq(0f))
  def kr(value: Double, values: Double*): ControlProxy      = kr(IIdxSeq((value.toFloat +: values.map(_.toFloat)): _*))
  def kr(value: Float,  values: Float* ): ControlProxy      = kr(IIdxSeq((value +: values): _*))

  def tr: TrigControlProxy = tr(IIdxSeq(0f))
  def tr(value: Double, values: Double*): TrigControlProxy  = tr(IIdxSeq((value.toFloat +: values.map(_.toFloat)): _*))
  def tr(value: Float,  values: Float* ): TrigControlProxy  = tr(IIdxSeq((value +: values): _*))

  def ar: AudioControlProxy = ar(IIdxSeq(0f))
  def ar(value: Double, values: Double*): AudioControlProxy = ar(IIdxSeq((value.toFloat +: values.map(_.toFloat)): _*))
  def ar(value: Float,  values: Float* ): AudioControlProxy = ar(IIdxSeq((value +: values): _*))

  @inline private def ir(values: IIdxSeq[Float]) = ControlProxy(scalar,  values, Some(name))(controlIrFactory)
  @inline private def kr(values: IIdxSeq[Float]) = ControlProxy(control, values, Some(name))(controlKrFactory)
  @inline private def tr(values: IIdxSeq[Float]) = TrigControlProxy     (values, Some(name))
  @inline private def ar(values: IIdxSeq[Float]) = AudioControlProxy    (values, Some(name))
}

sealed trait ControlFactoryLike[T] {
  type Proxy = T

  // don't ask me what this is doing. some vital variance correction...
  def build(b: UGenGraph.Builder, proxies: Proxy*): Map[ControlProxyLike[_], (UGen, Int)]
}

abstract class AbstractControlFactory[T <: AbstractControlProxy[T]] extends ControlFactoryLike[T] /* with Serializable */ {
  def build(b: UGenGraph.Builder, proxies: T*): Map[ControlProxyLike[_], (UGen, Int)] = {
    var numChannels = 0
    val specialIndex = proxies.map(p => {
      numChannels += p.values.size
      b.addControl(p.values, p.name)
    }).head
    val ugen = makeUGen(numChannels, specialIndex)
    var offset = 0
    proxies.map(p => {
      val res = p -> (ugen, offset)
      offset += p.values.size
      res
    })(breakOut)
  }

  protected def makeUGen(numChannels: Int, specialIndex: Int): UGen
}

sealed trait ControlProxyLike[Impl] extends GE {
  def rate: Rate
  def factory: ControlFactoryLike[Impl]
  def name: Option[String]
}

abstract class AbstractControlProxy[Impl] // ( numChannels: Int )
  extends ControlProxyLike[Impl] {

  // ---- constructor ----
  SynthGraph.builder.addControlProxy(this)

  def values: IIdxSeq[Float]

  def name: Option[String]

  /** Note: this expands to a single ControlUGenOutProxy for numChannels == 1,
    * otherwise to a sequence of proxies wrapped in UGenInGroup. Therefore,
    * {{{
    *    In.ar( "in".kr, 2 )
    * }}}
    * results in an `In` UGen, and doesn't rewrap into a UGenInGroup
    * (e.g. behaves like `In.ar( 0, 2 )` and not `In.ar( Seq( 0 ), 2 )` which
    * would mess up successive multi channel expansion.
    *
    * This is kind of a particular way of producing the proper `isWrapped` results.
    */
  final private[synth] def expand: UGenInLike = if (values.size == 1) {
    ControlUGenOutProxy(this, 0)
  } else {
    UGenInGroup(IIdxSeq.tabulate(values.size)(i => ControlUGenOutProxy(this, i)))
  }
}
