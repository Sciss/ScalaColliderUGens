/*
 *  Nyquist.scala
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

/** A helper graph element equivalent to `SampleRate.ir * 0.5`.
  *
  * ===Examples===
  *
  * {{{
  * // as frequency argument
  * play {
  *   Dust.ar(Nyquist()).pow(8)
  * }
  * }}}
  *
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  */
object Nyquist extends ProductReader[Nyquist] {
  def ir: Nyquist = new Nyquist()

  override def read(in: RefMapIn, key: String, arity: Int): Nyquist = {
    require (arity == 0)
    new Nyquist
  }
}

/** A helper graph element equivalent to `SampleRate.ir * 0.5`.
  *
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  */
final case class Nyquist() extends GE with ScalarRated {
  private[synth] def expand: UGenInLike = {
    val sr = UGen.SingleOut("SampleRate", scalar, Vector.empty)  // `SampleRate` is only defined in `core`
    BinaryOpUGen.Times.make(sr, 0.5f)
  }
}