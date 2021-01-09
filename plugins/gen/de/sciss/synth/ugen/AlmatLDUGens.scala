// revision: 4
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen based on Hopf bifurcations that tries to synchronize to an input signal.
  * 
  * ===Examples===
  * 
  * {{{
  * // sync to sine input
  * play {
  *   val freqIn  = MouseX.kr(100.0, 1000.0)
  *   freqIn.poll(label = "mouse")
  *   val sigIn   = SinOsc.ar(freqIn)
  *   val hopf    = Hopf.ar(sigIn, 0.001)
  *   val freqOut = hopf.omega * SampleRate.ir / (2*math.Pi)
  *   freqOut.poll(label = "hopf ")
  *   val sigOut  = hopf.phase
  *   Pan2.ar(sigOut * 0.01)
  * }
  * }}}
  * 
  * This is a third-party UGen (kuramoto).
  * 
  * @see [[de.sciss.synth.ugen.Kuramoto$ Kuramoto]]
  */
object Hopf extends ProductReader[Hopf] {
  /** @param in               Input signal (force) to track.
    * @param coupling         Strength of coupling.
    * @param radius           Limit cycle radius (amplitude attractor).
    */
  def ar(in: GE, coupling: GE, radius: GE = 1.0f): Hopf = new Hopf(audio, in, coupling, radius)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Hopf = {
    require (arity == 4)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _coupling = in.readGE()
    val _radius   = in.readGE()
    new Hopf(_rate, _in, _coupling, _radius)
  }
}

/** A UGen based on Hopf bifurcations that tries to synchronize to an input signal.
  * 
  * This is a third-party UGen (kuramoto).
  * 
  * @param in               Input signal (force) to track.
  * @param coupling         Strength of coupling.
  * @param radius           Limit cycle radius (amplitude attractor).
  * 
  * @see [[de.sciss.synth.ugen.Kuramoto$ Kuramoto]]
  */
final case class Hopf(rate: MaybeRate, in: GE, coupling: GE, radius: GE = 1.0f) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, coupling.expand, radius.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    val _args2 = matchRate(_args1, 1, _rate)
    val _args3 = matchRate(_args2, 2, _rate)
    UGen.MultiOut(name, _rate, Vector.fill(4)(_rate), _args3)
  }
  
  def x: GE = ChannelProxy(this, 0)
  
  def y: GE = ChannelProxy(this, 1)
  
  def omega: GE = ChannelProxy(this, 2)
  
  def phase: GE = ChannelProxy(this, 3)
}