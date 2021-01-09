/*
 *  Kuramoto.scala
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

/** A UGen that implements the Kuramoto model of synchronization of
  * coupled oscillators.
  *
  * ===Examples===
  *
  * {{{
  * // mouse-x controls coupling
  * play {
  *   val n = 10
  *   val phases = Kuramoto.ar(
  *     mode = 0,
  *     initPhase   = Seq.fill(n)(Rand(0.0, 3.14159)),
  *     incr        = Seq.fill(n)(Rand(0.02, 0.08)),
  *     intCoupling = Lag2.ar(MouseX.kr(1.0e-4, 1.0e-1, warp = 1), 0.1)
  * 	)
  * 	SplayAz.ar(2, SinOsc.ar(0, phases)) * 0.1
  * }
  * }}}
  *
  * @see [[de.sciss.synth.ugen.Hopf$ Hopf]]
  */
object Kuramoto extends ProductReader[Kuramoto] {
  /**
    * @param mode `0`: all oscillators are coupled; `1`: neighboring oscillators are coupled
    *             (they can be imagined to form a ring); `2`:  neighboring oscillators are negatively coupled;
    *             `3`: like `2` but discarding current phases (?); `4`: like `2` but using cosine instead
    *             of sine mapping
    */
  def ar(mode: GE = 0.0, initPhase: GE = 0.0, incr: GE = 0.0, extPhase: GE = 0.0,
         intCoupling: GE, extCoupling: GE = 0.0): Kuramoto =
    apply(audio, mode = mode, initPhase = initPhase, incr = incr, extPhase = extPhase,
      intCoupling = intCoupling, extCoupling = extCoupling)

  override def read(in: RefMapIn, prefix: String, arity: Int): Kuramoto = {
    require (arity == 7)
    val _rate         = in.readRate()
    val _mode         = in.readGE()
    val _initPhase    = in.readGE()
    val _incr         = in.readGE()
    val _extPhase     = in.readGE()
    val _intCoupling  = in.readGE()
    val _extCoupling  = in.readGE()
    new Kuramoto(_rate, _mode, _initPhase, _incr, _extPhase, _intCoupling, _extCoupling)
  }
}
/** A UGen that implements the Kuramoto model of synchronization of
  * coupled oscillators.
  *
  * @param mode `0`: all oscillators are coupled; `1`: neighboring oscillators are coupled
  *             (they can be imagined to form a ring); `2`:  neighboring oscillators are negatively coupled;
  *             `3`: like `2` but discarding current phases (?); `4`: like `2` but using cosine instead
  *             of sine mapping
  */
final case class Kuramoto(rate: Rate, mode: GE = 0.0, initPhase: GE = 0.0, incr: GE = 0.0, extPhase: GE = 0.0,
                          intCoupling: GE, extCoupling: GE = 0.0)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = {
    val args = Vector(initPhase.expand, incr.expand, extPhase.expand, intCoupling.expand, extCoupling.expand)
    var exp = 0
    args.foreach(_.unbubble match {
      case _: UGenIn =>
        exp = math.max(exp, 1)
      case g: UGenInGroup =>
        exp = math.max(exp, g.numOutputs)
    })

    val argsAlign = args.flatMap(a => Vector.tabulate(exp)(i => a.unwrap(i)))
    val args1     = mode.expand +: argsAlign
    UGenSource.unwrap(this, args1)
  }

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val argM1   = args.size - 1
    assert (argM1 % 5 == 0)
    val n       = argM1 / 5
    val _args0  = Constant(n) +: args
    val _args1  = UGenSource.matchRateFrom(_args0, 0, rate)
    UGen.MultiOut(name, rate, Vector.fill(n)(rate), _args1)
  }
}