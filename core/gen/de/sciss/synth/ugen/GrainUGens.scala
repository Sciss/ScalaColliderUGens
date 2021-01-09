// revision: 4
package de.sciss.synth
package ugen

import UGenSource._
object GrainIn extends ProductReader[GrainIn] {
  def ar(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, in: GE, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512): GrainIn = 
    new GrainIn(numChannels, trig, dur, in, pan, envBuf, maxGrains)
  
  def read(in: RefMapIn, arity: Int): GrainIn = {
    require (arity == 7)
    val _numChannels  = in.readInt()
    val _trig         = in.readGE()
    val _dur          = in.readGE()
    val _in           = in.readGE()
    val _pan          = in.readGE()
    val _envBuf       = in.readGE()
    val _maxGrains    = in.readGE()
    new GrainIn(_numChannels, _trig, _dur, _in, _pan, _envBuf, _maxGrains)
  }
}
final case class GrainIn(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, in: GE, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, dur.expand, in.expand, pan.expand, envBuf.expand, maxGrains.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}
object GrainSin extends ProductReader[GrainSin] {
  def ar(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, freq: GE = 440.0f, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512): GrainSin = 
    new GrainSin(numChannels, trig, dur, freq, pan, envBuf, maxGrains)
  
  def read(in: RefMapIn, arity: Int): GrainSin = {
    require (arity == 7)
    val _numChannels  = in.readInt()
    val _trig         = in.readGE()
    val _dur          = in.readGE()
    val _freq         = in.readGE()
    val _pan          = in.readGE()
    val _envBuf       = in.readGE()
    val _maxGrains    = in.readGE()
    new GrainSin(_numChannels, _trig, _dur, _freq, _pan, _envBuf, _maxGrains)
  }
}
final case class GrainSin(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, freq: GE = 440.0f, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, dur.expand, freq.expand, pan.expand, envBuf.expand, maxGrains.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}
object GrainFM extends ProductReader[GrainFM] {
  def ar(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, carFreq: GE = 440.0f, modFreq: GE = 200.0f, index: GE = 1.0f, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512): GrainFM = 
    new GrainFM(numChannels, trig, dur, carFreq, modFreq, index, pan, envBuf, maxGrains)
  
  def read(in: RefMapIn, arity: Int): GrainFM = {
    require (arity == 9)
    val _numChannels  = in.readInt()
    val _trig         = in.readGE()
    val _dur          = in.readGE()
    val _carFreq      = in.readGE()
    val _modFreq      = in.readGE()
    val _index        = in.readGE()
    val _pan          = in.readGE()
    val _envBuf       = in.readGE()
    val _maxGrains    = in.readGE()
    new GrainFM(_numChannels, _trig, _dur, _carFreq, _modFreq, _index, _pan, _envBuf, _maxGrains)
  }
}
final case class GrainFM(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, carFreq: GE = 440.0f, modFreq: GE = 200.0f, index: GE = 1.0f, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, dur.expand, carFreq.expand, modFreq.expand, index.expand, pan.expand, envBuf.expand, maxGrains.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}

/** A granular synthesis UGen taking sound stored in a buffer. Another buffer can
  * be used to provide an amplitude envelope. The input sound buffer must be
  * monophonic, but output may be multi-channel, using a panorama control.
  * 
  * All arguments except `numChannels` and `maxGrain` are polled at grain creation
  * (trigger) time.
  * 
  * @see [[de.sciss.synth.ugen.TGrains$ TGrains]]
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  */
object GrainBuf extends ProductReader[GrainBuf] {
  /** @param numChannels      the number of channels to output. If `1` , a monophonic
    *                         signal is returned and the `pan` argument is ignored.
    * @param trig             a control- or audio-rate trigger to start a new grain.
    *                         For audio-rate, timing is sample frame accurate.
    * @param dur              grain duration in seconds
    * @param buf              buffer holding a mono audio signal portions of which
    *                         are read as grains
    * @param speed            playback speed of the grain, where `1.0` is original
    *                         rate, `0.5` is half speed etc.
    * @param pos              grain start position, where `0` is beginning and `1` is
    *                         the end of the input buffer
    * @param interp           interpolation type when using pitch-shifting via
    *                         `speed` . `1` for no interpolation (nearest sample), `2`
    *                         for linear interpolation, and `4` for cubic
    *                         interpolation.
    * @param pan              panning position when `numChannels` is greater than
    *                         one. Equivalent to the pan position of `Pan2` (for
    *                         stereo output) or `PanAz` (for more than two channels)
    * @param envBuf           identifier of a buffer containing a signal to use for
    *                         the grain envelope. The default value of `-1` means that
    *                         a built-in Hann envelope is used.
    * @param maxGrains        maximum number of overlapping grains that can be used
    *                         at a given time. This value is set at the UGens init
    *                         time and cannot be modified later. Lower value mean that
    *                         less memory is used.
    */
  def ar(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, buf: GE, speed: GE = 1.0f, pos: GE = 0.0f, interp: GE = 2, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512): GrainBuf = 
    new GrainBuf(numChannels, trig, dur, buf, speed, pos, interp, pan, envBuf, maxGrains)
  
  def read(in: RefMapIn, arity: Int): GrainBuf = {
    require (arity == 10)
    val _numChannels  = in.readInt()
    val _trig         = in.readGE()
    val _dur          = in.readGE()
    val _buf          = in.readGE()
    val _speed        = in.readGE()
    val _pos          = in.readGE()
    val _interp       = in.readGE()
    val _pan          = in.readGE()
    val _envBuf       = in.readGE()
    val _maxGrains    = in.readGE()
    new GrainBuf(_numChannels, _trig, _dur, _buf, _speed, _pos, _interp, _pan, _envBuf, _maxGrains)
  }
}

/** A granular synthesis UGen taking sound stored in a buffer. Another buffer can
  * be used to provide an amplitude envelope. The input sound buffer must be
  * monophonic, but output may be multi-channel, using a panorama control.
  * 
  * All arguments except `numChannels` and `maxGrain` are polled at grain creation
  * (trigger) time.
  * 
  * @param numChannels      the number of channels to output. If `1` , a monophonic
  *                         signal is returned and the `pan` argument is ignored.
  * @param trig             a control- or audio-rate trigger to start a new grain.
  *                         For audio-rate, timing is sample frame accurate.
  * @param dur              grain duration in seconds
  * @param buf              buffer holding a mono audio signal portions of which
  *                         are read as grains
  * @param speed            playback speed of the grain, where `1.0` is original
  *                         rate, `0.5` is half speed etc.
  * @param pos              grain start position, where `0` is beginning and `1` is
  *                         the end of the input buffer
  * @param interp           interpolation type when using pitch-shifting via
  *                         `speed` . `1` for no interpolation (nearest sample), `2`
  *                         for linear interpolation, and `4` for cubic
  *                         interpolation.
  * @param pan              panning position when `numChannels` is greater than
  *                         one. Equivalent to the pan position of `Pan2` (for
  *                         stereo output) or `PanAz` (for more than two channels)
  * @param envBuf           identifier of a buffer containing a signal to use for
  *                         the grain envelope. The default value of `-1` means that
  *                         a built-in Hann envelope is used.
  * @param maxGrains        maximum number of overlapping grains that can be used
  *                         at a given time. This value is set at the UGens init
  *                         time and cannot be modified later. Lower value mean that
  *                         less memory is used.
  * 
  * @see [[de.sciss.synth.ugen.TGrains$ TGrains]]
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  */
final case class GrainBuf(numChannels: Int = 1, trig: GE, dur: GE = 1.0f, buf: GE, speed: GE = 1.0f, pos: GE = 0.0f, interp: GE = 2, pan: GE = 0.0f, envBuf: GE = -1, maxGrains: GE = 512)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, dur.expand, buf.expand, speed.expand, pos.expand, interp.expand, pan.expand, envBuf.expand, maxGrains.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}
object Warp1 extends ProductReader[Warp1] {
  def ar(numChannels: Int, buf: GE, pos: GE = 0.0f, speed: GE = 1.0f, winSize: GE = 0.2f, envBuf: GE = -1, overlaps: GE = 8.0f, winRand: GE = 0.0f, interp: GE = 1): Warp1 = 
    new Warp1(numChannels, buf, pos, speed, winSize, envBuf, overlaps, winRand, interp)
  
  def read(in: RefMapIn, arity: Int): Warp1 = {
    require (arity == 9)
    val _numChannels  = in.readInt()
    val _buf          = in.readGE()
    val _pos          = in.readGE()
    val _speed        = in.readGE()
    val _winSize      = in.readGE()
    val _envBuf       = in.readGE()
    val _overlaps     = in.readGE()
    val _winRand      = in.readGE()
    val _interp       = in.readGE()
    new Warp1(_numChannels, _buf, _pos, _speed, _winSize, _envBuf, _overlaps, _winRand, _interp)
  }
}
final case class Warp1(numChannels: Int, buf: GE, pos: GE = 0.0f, speed: GE = 1.0f, winSize: GE = 0.2f, envBuf: GE = -1, overlaps: GE = 8.0f, winRand: GE = 0.0f, interp: GE = 1)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, pos.expand, speed.expand, winSize.expand, envBuf.expand, overlaps.expand, winRand.expand, interp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}