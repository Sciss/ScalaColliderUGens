// revision: 6
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen for Vector Base Amplitude Panning (VBAP). This allows for equal power
  * panning of a source over an arbitrary array of equidistant speakers. Normally
  * this would be a ring, a dome, or partial dome.
  * 
  * VBAP was created by Ville Pulkki. For more information on VBAP see
  * http://www.acoustics.hut.fi/research/cat/vbap/ This version of VBAP for SC was
  * ported from the ver. 0.99 PD code by Scott Wilson.
  * 
  * ===Examples===
  * 
  * {{{
  * // two-dimensional
  * val a = VBAPSetup(2, Seq(0, 45, 90, 135, 180, -135, -90, -45)) // 8 channel ring
  * 
  * val b = Buffer.alloc(s, a.bufferData.size)
  * b.setn(a.bufferData)
  * 
  * val x = play {
  *   val azi = "azi".kr(0)
  *   val ele = "ele".kr(0)
  *   val spr = "spr".kr(0)
  *   VBAP.ar(8, PinkNoise.ar(0.2), b.id, azi, ele, spr)
  * }
  * 
  * // test them out
  * x.set("azi" -> a.directions(1).azi)
  * x.set("azi" -> a.directions(2).azi)
  * x.set("azi" -> a.directions(3).azi)
  * // ...
  * x.set("azi" -> a.directions(7).azi)
  * x.set("azi" -> a.directions(0).azi)
  * 
  * // try the spread
  * x.set("spr" ->  20)
  * x.set("spr" -> 100) // all speakers
  * 
  * x.free(); b.free();
  * }}}
  * 
  * This is a third-party UGen (VBAPUGens).
  * 
  * @see [[de.sciss.synth.ugen.CircleRamp$ CircleRamp]]
  */
object VBAP extends ProductReader[VBAP] {
  /** @param numChannels      the number of output channels
    * @param in               the signal to be panned
    * @param buf              id of a buffer containing data calculated by
    *                         `VBAPSetup` . Its number of channels must correspond to
    *                         `numChannels`
    * @param azimuth          +/- 180° from the median plane (i.e. straight ahead)
    * @param elevation        +/- 90° from the azimuth plane
    * @param spread           A value from 0-100. When 0, if the signal is panned
    *                         exactly to a speaker location the signal is only on that
    *                         speaker. At values higher than 0, the signal will always
    *                         be on more than one speaker. This can smooth the panning
    *                         effect by making localisation blur more constant.
    */
  def kr(numChannels: Int, in: GE, buf: GE, azimuth: GE = 0, elevation: GE = 1, spread: GE = 0): VBAP = 
    new VBAP(control, numChannels, in, buf, azimuth, elevation, spread)
  
  /** @param numChannels      the number of output channels
    * @param in               the signal to be panned
    * @param buf              id of a buffer containing data calculated by
    *                         `VBAPSetup` . Its number of channels must correspond to
    *                         `numChannels`
    * @param azimuth          +/- 180° from the median plane (i.e. straight ahead)
    * @param elevation        +/- 90° from the azimuth plane
    * @param spread           A value from 0-100. When 0, if the signal is panned
    *                         exactly to a speaker location the signal is only on that
    *                         speaker. At values higher than 0, the signal will always
    *                         be on more than one speaker. This can smooth the panning
    *                         effect by making localisation blur more constant.
    */
  def ar(numChannels: Int, in: GE, buf: GE, azimuth: GE = 0, elevation: GE = 1, spread: GE = 0): VBAP = 
    new VBAP(audio, numChannels, in, buf, azimuth, elevation, spread)
  
  def read(in: RefMapIn, key: String, arity: Int): VBAP = {
    require (arity == 7)
    val _rate         = in.readMaybeRate()
    val _numChannels  = in.readInt()
    val _in           = in.readGE()
    val _buf          = in.readGE()
    val _azimuth      = in.readGE()
    val _elevation    = in.readGE()
    val _spread       = in.readGE()
    new VBAP(_rate, _numChannels, _in, _buf, _azimuth, _elevation, _spread)
  }
}

/** A UGen for Vector Base Amplitude Panning (VBAP). This allows for equal power
  * panning of a source over an arbitrary array of equidistant speakers. Normally
  * this would be a ring, a dome, or partial dome.
  * 
  * VBAP was created by Ville Pulkki. For more information on VBAP see
  * http://www.acoustics.hut.fi/research/cat/vbap/ This version of VBAP for SC was
  * ported from the ver. 0.99 PD code by Scott Wilson.
  * 
  * This is a third-party UGen (VBAPUGens).
  * 
  * @param numChannels      the number of output channels
  * @param in               the signal to be panned
  * @param buf              id of a buffer containing data calculated by
  *                         `VBAPSetup` . Its number of channels must correspond to
  *                         `numChannels`
  * @param azimuth          +/- 180° from the median plane (i.e. straight ahead)
  * @param elevation        +/- 90° from the azimuth plane
  * @param spread           A value from 0-100. When 0, if the signal is panned
  *                         exactly to a speaker location the signal is only on that
  *                         speaker. At values higher than 0, the signal will always
  *                         be on more than one speaker. This can smooth the panning
  *                         effect by making localisation blur more constant.
  * 
  * @see [[de.sciss.synth.ugen.CircleRamp$ CircleRamp]]
  */
final case class VBAP(rate: MaybeRate, numChannels: Int, in: GE, buf: GE, azimuth: GE = 0, elevation: GE = 1, spread: GE = 0)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, buf.expand, azimuth.expand, elevation.expand, spread.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.MultiOut(name, _rate, Vector.fill(numChannels)(_rate), _args1)
  }
}

/** This is a UGen like `Ramp` , but it always takes the shortest way around a
  * defined circle, wrapping values where appropriate. This can be useful when
  * smoothing panning signals for speaker rings, for instance in Vector Base
  * Amplitude Panning.
  * 
  * This is a third-party UGen (VBAPUGens).
  * 
  * @see [[de.sciss.synth.ugen.VBAP$ VBAP]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
object CircleRamp extends ProductReader[CircleRamp] {
  /** @param in               The signal to be smoothed.
    * @param dur              Ramp duration in seconds
    * @param lo               The lower wrap value
    * @param hi               The upper wrap value
    */
  def kr(in: GE, dur: GE = 0.1f, lo: GE = -180, hi: GE = 180): CircleRamp = 
    new CircleRamp(control, in, dur, lo, hi)
  
  /** @param in               The signal to be smoothed.
    * @param dur              Ramp duration in seconds
    * @param lo               The lower wrap value
    * @param hi               The upper wrap value
    */
  def ar(in: GE, dur: GE = 0.1f, lo: GE = -180, hi: GE = 180): CircleRamp = 
    new CircleRamp(audio, in, dur, lo, hi)
  
  def read(in: RefMapIn, key: String, arity: Int): CircleRamp = {
    require (arity == 5)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _dur  = in.readGE()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    new CircleRamp(_rate, _in, _dur, _lo, _hi)
  }
}

/** This is a UGen like `Ramp` , but it always takes the shortest way around a
  * defined circle, wrapping values where appropriate. This can be useful when
  * smoothing panning signals for speaker rings, for instance in Vector Base
  * Amplitude Panning.
  * 
  * This is a third-party UGen (VBAPUGens).
  * 
  * @param in               The signal to be smoothed.
  * @param dur              Ramp duration in seconds
  * @param lo               The lower wrap value
  * @param hi               The upper wrap value
  * 
  * @see [[de.sciss.synth.ugen.VBAP$ VBAP]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
final case class CircleRamp(rate: MaybeRate, in: GE, dur: GE = 0.1f, lo: GE = -180, hi: GE = 180)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, dur.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}