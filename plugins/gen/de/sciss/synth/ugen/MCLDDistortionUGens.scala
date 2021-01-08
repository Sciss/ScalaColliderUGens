// revision: 2
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that produces distortion by subtracting the input signal's magnitude
  * from 1.
  * 
  * If the input is positive, it outputs (+1 - input). If the input is negative, it
  * outputs (-1 - input).
  * 
  * ===Examples===
  * 
  * {{{
  * // sine plus noise
  * play { InsideOut.ar(SinOsc.ar(220) + PinkNoise.ar(0.9)) * 0.1 }
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  */
object InsideOut extends Reader[InsideOut] {
  /** @param in               input signal to be distorted
    */
  def kr(in: GE): InsideOut = new InsideOut(control, in)
  
  /** @param in               input signal to be distorted
    */
  def ar(in: GE): InsideOut = new InsideOut(audio, in)
  
  def read(in: DataInput): InsideOut = {
    readArity(in, 2)
    val _rate = readMaybeRate(in)
    val _in   = readGE(in)
    new InsideOut(_rate, _in)
  }
}

/** A UGen that produces distortion by subtracting the input signal's magnitude
  * from 1.
  * 
  * If the input is positive, it outputs (+1 - input). If the input is negative, it
  * outputs (-1 - input).
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param in               input signal to be distorted
  */
final case class InsideOut(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen which uses zero-crossings to divide an input signal into tiny segments.
  * It simply discards a fraction of the segments (replacing them with silence).
  * 
  * The technique was described in a lecture by Trevor Wishart.
  * 
  * ===Examples===
  * 
  * {{{
  * // sine plus noise
  * play {
  *   val sig  = (SinOsc.ar + PinkNoise.ar) * 0.5
  *   val mode = MouseY.kr(1, 2).roundTo(1)
  *   WaveLoss.ar(sig, drop = MouseX.kr(0, 40), chunk = 40, mode = mode) * 0.1
  * }
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  */
object WaveLoss extends Reader[WaveLoss] {
  /** @param in               input signal to be distorted
    * @param drop             the number of wave segments to drop in each group of
    *                         size `chunk` .
    * @param chunk            the number of wave segments that are grouped, so that
    *                         `drop` elements from it are removed.
    * @param mode             `1` for deterministic mode, in which always the first
    *                         `drop` segments within a `chunk` are dropped, `2` for
    *                         randomized mode, where `drop` segments at random indices
    *                         within a `chunk` are dropped.
    */
  def kr(in: GE, drop: GE = 20, chunk: GE = 40, mode: GE = 1): WaveLoss = 
    new WaveLoss(control, in, drop, chunk, mode)
  
  /** @param in               input signal to be distorted
    * @param drop             the number of wave segments to drop in each group of
    *                         size `chunk` .
    * @param chunk            the number of wave segments that are grouped, so that
    *                         `drop` elements from it are removed.
    * @param mode             `1` for deterministic mode, in which always the first
    *                         `drop` segments within a `chunk` are dropped, `2` for
    *                         randomized mode, where `drop` segments at random indices
    *                         within a `chunk` are dropped.
    */
  def ar(in: GE, drop: GE = 20, chunk: GE = 40, mode: GE = 1): WaveLoss = 
    new WaveLoss(audio, in, drop, chunk, mode)
  
  def read(in: DataInput): WaveLoss = {
    readArity(in, 5)
    val _rate   = readMaybeRate(in)
    val _in     = readGE(in)
    val _drop   = readGE(in)
    val _chunk  = readGE(in)
    val _mode   = readGE(in)
    new WaveLoss(_rate, _in, _drop, _chunk, _mode)
  }
}

/** A UGen which uses zero-crossings to divide an input signal into tiny segments.
  * It simply discards a fraction of the segments (replacing them with silence).
  * 
  * The technique was described in a lecture by Trevor Wishart.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param in               input signal to be distorted
  * @param drop             the number of wave segments to drop in each group of
  *                         size `chunk` .
  * @param chunk            the number of wave segments that are grouped, so that
  *                         `drop` elements from it are removed.
  * @param mode             `1` for deterministic mode, in which always the first
  *                         `drop` segments within a `chunk` are dropped, `2` for
  *                         randomized mode, where `drop` segments at random indices
  *                         within a `chunk` are dropped.
  */
final case class WaveLoss(rate: MaybeRate, in: GE, drop: GE = 20, chunk: GE = 40, mode: GE = 1)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, drop.expand, chunk.expand, mode.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1, isIndividual = true)
  }
}

/** A UGen implementing a simplistic pitch-raising algorithm. It is not meant to
  * sound natural, and its sound is reminiscent of some weird mixture of filter,
  * ring-modulator and pitch-shifter, depending on the input.
  * 
  * The algorithm works by cutting the signal into fragments (delimited by
  * upwards-going zero-crossings) and squeezing those fragments in the time domain
  * (i.e. simply playing them back faster than they came in), leaving silences in
  * between.
  * 
  * ===Examples===
  * 
  * {{{
  * // trigger grains
  * play { Squiz.ar(SinOsc.ar(440), MouseX.kr(1, 10, 1), zeroCrossings = MouseY.kr(1, 10)) * 0.1 }
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  */
object Squiz extends Reader[Squiz] {
  /** @param in               input signal to be distorted
    * @param pitchRatio       the ratio by which pitch will be raised, e.g. the
    *                         default value of 2 will raise by one octave. Only
    *                         upwards pitch-shifts are possible so a value below 1 has
    *                         no effect.
    * @param zeroCrossings    how many positive-going zero-crossings are used to
    *                         delimit a chunk.
    * @param maxDur           the maximum duration to remember each fragment,
    *                         corresponding with an internally allocated memory.
    *                         Raising it higher will use more real-time memory and
    *                         probably will not sound very different (especially if
    *                         `zeroCrossings` is low). ''(init-time only)''
    */
  def kr(in: GE, pitchRatio: GE = 2, zeroCrossings: GE = 1, maxDur: GE = 0.1f): Squiz = 
    new Squiz(control, in, pitchRatio, zeroCrossings, maxDur)
  
  /** @param in               input signal to be distorted
    * @param pitchRatio       the ratio by which pitch will be raised, e.g. the
    *                         default value of 2 will raise by one octave. Only
    *                         upwards pitch-shifts are possible so a value below 1 has
    *                         no effect.
    * @param zeroCrossings    how many positive-going zero-crossings are used to
    *                         delimit a chunk.
    * @param maxDur           the maximum duration to remember each fragment,
    *                         corresponding with an internally allocated memory.
    *                         Raising it higher will use more real-time memory and
    *                         probably will not sound very different (especially if
    *                         `zeroCrossings` is low). ''(init-time only)''
    */
  def ar(in: GE, pitchRatio: GE = 2, zeroCrossings: GE = 1, maxDur: GE = 0.1f): Squiz = 
    new Squiz(audio, in, pitchRatio, zeroCrossings, maxDur)
  
  def read(in: DataInput): Squiz = {
    readArity(in, 5)
    val _rate           = readRate(in)
    val _in             = readGE(in)
    val _pitchRatio     = readGE(in)
    val _zeroCrossings  = readGE(in)
    val _maxDur         = readGE(in)
    new Squiz(_rate, _in, _pitchRatio, _zeroCrossings, _maxDur)
  }
}

/** A UGen implementing a simplistic pitch-raising algorithm. It is not meant to
  * sound natural, and its sound is reminiscent of some weird mixture of filter,
  * ring-modulator and pitch-shifter, depending on the input.
  * 
  * The algorithm works by cutting the signal into fragments (delimited by
  * upwards-going zero-crossings) and squeezing those fragments in the time domain
  * (i.e. simply playing them back faster than they came in), leaving silences in
  * between.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param in               input signal to be distorted
  * @param pitchRatio       the ratio by which pitch will be raised, e.g. the
  *                         default value of 2 will raise by one octave. Only
  *                         upwards pitch-shifts are possible so a value below 1 has
  *                         no effect.
  * @param zeroCrossings    how many positive-going zero-crossings are used to
  *                         delimit a chunk.
  * @param maxDur           the maximum duration to remember each fragment,
  *                         corresponding with an internally allocated memory.
  *                         Raising it higher will use more real-time memory and
  *                         probably will not sound very different (especially if
  *                         `zeroCrossings` is low). ''(init-time only)''
  */
final case class Squiz(rate: Rate, in: GE, pitchRatio: GE = 2, zeroCrossings: GE = 1, maxDur: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, pitchRatio.expand, zeroCrossings.expand, maxDur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}