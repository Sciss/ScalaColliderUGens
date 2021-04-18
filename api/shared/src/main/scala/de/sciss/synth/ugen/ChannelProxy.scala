/*
 *  ChannelProxy.scala
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

import UGenSource._

object ChannelProxy extends ProductType[ChannelProxy] {
  final val typeId = 401

  override def read(in: RefMapIn, key: String, arity: Int): ChannelProxy = {
    require (arity == 2)
    val _elem   = in.readGE()
    val _index  = in.readInt()
    new ChannelProxy(_elem, _index)
  }
}

/** A helper graph element that selects a particular output channel of
  * another element. The index is an `Integer` and thus cannot be
  * determined at graph expansion time. If this is desired, the
  * `Select` UGen can be used.
  *
  * Usually the graph element operator `\` (backlash) can be used
  * instead of explicitly writing `ChannelProxy`. Thus
  * `elem \ index` is equivalent to `ChannelProxy(elem, index)`.
  * UGens with a fixed number of outputs often have auxiliary methods
  * to access the channels in meaningful way, e.g. `Pitch` has
  * method `freq` and `hasFreq`, which are equivalent to
  * `pitch \ 0` and `pitch \ 1`.
  *
  * Because ScalaCollider allows late-expanding
  * graph elements, we have no direct way to get some
  * array of a UGen's outputs.
  *
  * ===Examples===
  *
  * {{{
  * // frequency of a pitch estimator
  * play {
  *   val sin = SinOsc.ar(MouseX.kr(10, 10000, warp = 1))
  *   val pch = Pitch.kr(sin)
  *   val freq = pch \ 0  // same as pch.freq
  *   freq.poll(label = "freq")
  *   ()
  * }
  * }}}
  *
  * @param elem     a multi-channel element from which to select a channel.
  * @param index    channel index starting at zero. It automatically wraps around
  *                 the actual number of channels the input `elem` expands to.
  *
  * @see [[de.sciss.synth.ugen.NumChannels NumChannels]]
  * @see [[de.sciss.synth.ugen.Select$ Select]]
  */
final case class ChannelProxy(elem: GE, index: Int) extends GE.Lazy {
  def rate: MaybeRate = elem.rate

  override def toString = s"$elem.\\($index)"

  protected def makeUGens: UGenInLike = {
    // val _elem = elem.expand
    // _elem.unwrap(index)

    val res: UGenInLike = elem match {
      case in: UGenIn     => in
      case GESeq    (xs)  => xs(index % xs.length).expand
      case UGenInSeq(xs)  => xs(index % xs.length)
      case _ =>
        val _elem = elem.expand
        val out   = _elem.outputs
        val outF  = _elem.flatOutputs
        if (out.size == outF.size) {
          outF(index % outF.size)
        } else {
          UGenInGroup(Vector.tabulate(outF.size / out.size)(i => outF((index + i * out.size) % outF.size)))
        }
    }
    res
  }
}