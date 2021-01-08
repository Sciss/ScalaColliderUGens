// revision: 12
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that reports the server's current control rate. This is equivalent to
  * the reciprocal of `ControlDur`
  * 
  * ===Examples===
  * 
  * {{{
  * // print the control rate
  * play { ControlRate.ir.poll(0) }
  * }}}
  * {{{
  * // play a sine tone at control rate
  * play { SinOsc.ar(ControlRate.ir) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  */
object ControlRate extends Reader[ControlRate] {
  /** 
    */
  def ir: ControlRate = new ControlRate()
  
  def read(in: DataInput): ControlRate = {
    readArity(in, 0)
    new ControlRate()
  }
}

/** A UGen that reports the server's current control rate. This is equivalent to
  * the reciprocal of `ControlDur`
  * 
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  */
final case class ControlRate() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** A UGen that reports the server's current (audio) sample rate. This is
  * equivalent to the reciprocal of `SampleDur`
  * 
  * ===Examples===
  * 
  * {{{
  * // print the sample rate
  * play { SampleRate.ir.poll(0) }
  * }}}
  * {{{
  * // use a fraction as oscillator frequency
  * play {
  *   val div    = MouseX.kr(512, 2, 1, 0).roundTo(1)
  *   val change = HPZ1.kr(div).abs
  *   val freq   = SampleRate.ir / div
  *   freq.poll(change, label = "freq")
  *   SinOsc.ar(freq) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  * @see [[de.sciss.synth.ugen.ControlRate$ ControlRate]]
  * @see [[de.sciss.synth.ugen.RadiansPerSample$ RadiansPerSample]]
  */
object SampleRate extends Reader[SampleRate] {
  /** 
    */
  def ir: SampleRate = new SampleRate()
  
  def read(in: DataInput): SampleRate = {
    readArity(in, 0)
    new SampleRate()
  }
}

/** A UGen that reports the server's current (audio) sample rate. This is
  * equivalent to the reciprocal of `SampleDur`
  * 
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  * @see [[de.sciss.synth.ugen.ControlRate$ ControlRate]]
  * @see [[de.sciss.synth.ugen.RadiansPerSample$ RadiansPerSample]]
  */
final case class SampleRate() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** A UGen that reports the server's current (audio) sample period in seconds. This
  * is equivalent to the reciprocal of `SampleRate`
  * 
  * ===Examples===
  * 
  * {{{
  * // print the sample period
  * play { SampleDur.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
object SampleDur extends Reader[SampleDur] {
  /** 
    */
  def ir: SampleDur = new SampleDur()
  
  def read(in: DataInput): SampleDur = {
    readArity(in, 0)
    new SampleDur()
  }
}

/** A UGen that reports the server's current (audio) sample period in seconds. This
  * is equivalent to the reciprocal of `SampleRate`
  * 
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
final case class SampleDur() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** A UGen that reports the server's current control period in seconds. This is
  * equivalent to the reciprocal of `ControlRate`
  * 
  * ===Examples===
  * 
  * {{{
  * // print the control period
  * play { ControlDur.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.ControlRate$ ControlRate]]
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  */
object ControlDur extends Reader[ControlDur] {
  /** 
    */
  def ir: ControlDur = new ControlDur()
  
  def read(in: DataInput): ControlDur = {
    readArity(in, 0)
    new ControlDur()
  }
}

/** A UGen that reports the server's current control period in seconds. This is
  * equivalent to the reciprocal of `ControlRate`
  * 
  * @see [[de.sciss.synth.ugen.ControlRate$ ControlRate]]
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  */
final case class ControlDur() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** A UGen that reports the fractional sample offset of the current Synth from its
  * requested scheduled start.
  * 
  * When a synth is created from a time stamped osc-bundle, it starts calculation
  * at the next possible block (normally 64 samples). Using an `OffsetOut` UGen, one
  * can delay the audio so that it matches sample accurately.
  * 
  * For some synthesis methods, one even needs subsample accuracy.
  * `SubsampleOffset` provides the information where, within the current sample, the
  * synth was scheduled. It can be used to offset envelopes or resample the audio
  * output.
  * 
  * ===Examples===
  * 
  * {{{
  * // print offset
  * SynthDef.recv("SubsampleOffset") {
  *   SubsampleOffset.ir.poll(0, "offset")
  * }
  * 
  * // create 2 pulse trains 1 sample apart, move one relatively to the other.
  * // when cursor is at the left, the impulses are adjacent, on the right, they are
  * // exactly 1 sample apart.
  * 
  * val dt = s.sampleRate.reciprocal // 1 sample delay
  * 
  * val x1, x2 = Synth(s)
  * 
  * // needed to use System.currentTimeMillis with osc.Bundle.secs
  * val SECONDS_FROM_1900_TO_1970 = 2208988800L
  * 
  * // We create two identical synths with a delay of half a sample,
  * // they should thus report offsets 0.5 apart (plus some floating point noise).
  * 
  * // execute the following three lines together!
  * val t0 = System.currentTimeMillis * 0.001 + SECONDS_FROM_1900_TO_1970
  * s ! osc.Bundle.secs(t0 + 0.2        , x1.newMsg("SubsampleOffset"))
  * s ! osc.Bundle.secs(t0 + 0.2 + dt/2 , x2.newMsg("SubsampleOffset"))
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.ControlRate$ ControlRate]]
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  * @see [[de.sciss.synth.ugen.OffsetOut$ OffsetOut]]
  */
object SubsampleOffset extends Reader[SubsampleOffset] {
  /** 
    */
  def ir: SubsampleOffset = new SubsampleOffset()
  
  def read(in: DataInput): SubsampleOffset = {
    readArity(in, 0)
    new SubsampleOffset()
  }
}

/** A UGen that reports the fractional sample offset of the current Synth from its
  * requested scheduled start.
  * 
  * When a synth is created from a time stamped osc-bundle, it starts calculation
  * at the next possible block (normally 64 samples). Using an `OffsetOut` UGen, one
  * can delay the audio so that it matches sample accurately.
  * 
  * For some synthesis methods, one even needs subsample accuracy.
  * `SubsampleOffset` provides the information where, within the current sample, the
  * synth was scheduled. It can be used to offset envelopes or resample the audio
  * output.
  * 
  * @see [[de.sciss.synth.ugen.ControlRate$ ControlRate]]
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  * @see [[de.sciss.synth.ugen.OffsetOut$ OffsetOut]]
  */
final case class SubsampleOffset() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** A UGen that delivers the conversion factor from frequency in Hertz to radians
  * (normalized frequency). The relation is `RadiansPerSample * sr = 2pi` , thus
  * multiplying the UGen with a frequency between zero and nyquist (sr/2) yields the
  * normalized frequency between zero and pi.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { RadiansPerSample.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  */
object RadiansPerSample extends Reader[RadiansPerSample] {
  /** 
    */
  def ir: RadiansPerSample = new RadiansPerSample()
  
  def read(in: DataInput): RadiansPerSample = {
    readArity(in, 0)
    new RadiansPerSample()
  }
}

/** A UGen that delivers the conversion factor from frequency in Hertz to radians
  * (normalized frequency). The relation is `RadiansPerSample * sr = 2pi` , thus
  * multiplying the UGen with a frequency between zero and nyquist (sr/2) yields the
  * normalized frequency between zero and pi.
  * 
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  */
final case class RadiansPerSample() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Number of input buses.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NumInputBuses.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
object NumInputBuses extends Reader[NumInputBuses] {
  /** 
    */
  def ir: NumInputBuses = new NumInputBuses()
  
  def read(in: DataInput): NumInputBuses = {
    readArity(in, 0)
    new NumInputBuses()
  }
}

/** Number of input buses.
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
final case class NumInputBuses() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Number of output buses.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NumOutputBuses.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
object NumOutputBuses extends Reader[NumOutputBuses] {
  /** 
    */
  def ir: NumOutputBuses = new NumOutputBuses()
  
  def read(in: DataInput): NumOutputBuses = {
    readArity(in, 0)
    new NumOutputBuses()
  }
}

/** Number of output buses.
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
final case class NumOutputBuses() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Number of audio buses.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NumAudioBuses.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
object NumAudioBuses extends Reader[NumAudioBuses] {
  /** 
    */
  def ir: NumAudioBuses = new NumAudioBuses()
  
  def read(in: DataInput): NumAudioBuses = {
    readArity(in, 0)
    new NumAudioBuses()
  }
}

/** Number of audio buses.
  * 
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
final case class NumAudioBuses() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Number of control buses.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NumControlBuses.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
object NumControlBuses extends Reader[NumControlBuses] {
  /** 
    */
  def ir: NumControlBuses = new NumControlBuses()
  
  def read(in: DataInput): NumControlBuses = {
    readArity(in, 0)
    new NumControlBuses()
  }
}

/** Number of control buses.
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
final case class NumControlBuses() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Maximum number of audio buffers.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NumBuffers.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
object NumBuffers extends Reader[NumBuffers] {
  /** 
    */
  def ir: NumBuffers = new NumBuffers()
  
  def read(in: DataInput): NumBuffers = {
    readArity(in, 0)
    new NumBuffers()
  }
}

/** Maximum number of audio buffers.
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
final case class NumBuffers() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Identifier of the node which contains the UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NodeID.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.Poll$ Poll]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
object NodeID extends Reader[NodeID] {
  /** 
    */
  def ir: NodeID = new NodeID()
  
  def read(in: DataInput): NodeID = {
    readArity(in, 0)
    new NodeID()
  }
}

/** Identifier of the node which contains the UGen.
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.Poll$ Poll]]
  * @see [[de.sciss.synth.ugen.NumRunningSynths$ NumRunningSynths]]
  */
final case class NodeID() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Number of currently running synths.
  * 
  * ===Examples===
  * 
  * {{{
  * // print the UGen value
  * play { NumRunningSynths.ir.poll(0) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  */
object NumRunningSynths extends Reader[NumRunningSynths] {
  /** 
    */
  def ir: NumRunningSynths = new NumRunningSynths()
  
  def read(in: DataInput): NumRunningSynths = {
    readArity(in, 0)
    new NumRunningSynths()
  }
}

/** Number of currently running synths.
  * 
  * @see [[de.sciss.synth.ugen.NumAudioBuses$ NumAudioBuses]]
  * @see [[de.sciss.synth.ugen.NumControlBuses$ NumControlBuses]]
  * @see [[de.sciss.synth.ugen.NumBuffers$ NumBuffers]]
  * @see [[de.sciss.synth.ugen.NumInputBuses$ NumInputBuses]]
  * @see [[de.sciss.synth.ugen.NumOutputBuses$ NumOutputBuses]]
  */
final case class NumRunningSynths() extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args)
}

/** Returns the buffer's current sample rate.
  * 
  * ===Examples===
  * 
  * {{{
  * // rate of local buffer
  * play {
  *   val buf = LocalBuf(1024)
  *   BufSampleRate.ir(buf).poll(0) // matches server sample rate
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  */
object BufSampleRate extends Reader[BufSampleRate] {
  /** @param buf              Buffer id.
    */
  def ir(buf: GE): BufSampleRate = new BufSampleRate(scalar, buf)
  
  /** @param buf              Buffer id.
    */
  def kr(buf: GE): BufSampleRate = new BufSampleRate(control, buf)
  
  def read(in: DataInput): BufSampleRate = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    new BufSampleRate(_rate, _buf)
  }
}

/** Returns the buffer's current sample rate.
  * 
  * @param buf              Buffer id.
  * 
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  */
final case class BufSampleRate(rate: Rate, buf: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Returns a ratio by which the playback of the buffer at the provided index is to
  * be scaled relative to the current sample rate of the server.
  * {{{
  * buffer sample rate / server sample rate
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // rate scale of local buffer
  * play {
  *   val buf = LocalBuf(1024)
  *   BufRateScale.ir(buf).poll(0) // reports 1.0 because buffer rate matches server rate
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufSampleRate$ BufSampleRate]]
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  */
object BufRateScale extends Reader[BufRateScale] {
  /** @param buf              Buffer id.
    */
  def ir(buf: GE): BufRateScale = new BufRateScale(scalar, buf)
  
  /** @param buf              Buffer id.
    */
  def kr(buf: GE): BufRateScale = new BufRateScale(control, buf)
  
  def read(in: DataInput): BufRateScale = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    new BufRateScale(_rate, _buf)
  }
}

/** Returns a ratio by which the playback of the buffer at the provided index is to
  * be scaled relative to the current sample rate of the server.
  * {{{
  * buffer sample rate / server sample rate
  * }}}
  * 
  * 
  * @param buf              Buffer id.
  * 
  * @see [[de.sciss.synth.ugen.BufSampleRate$ BufSampleRate]]
  * @see [[de.sciss.synth.ugen.SampleRate$ SampleRate]]
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  */
final case class BufRateScale(rate: Rate, buf: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Returns the current number of allocated samples in the Buffer at the provided
  * index. A sample is not the same as a frame (compare with
  * [[de.sciss.synth.ugen.BufFrames$ BufFrames]] ); a frame includes the samples in
  * each channel of the buffer. Only for a mono buffer are samples the same as
  * frames.
  * {{{
  * samples = frames * numChannels
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // samples of local buffer
  * play {
  *   val buf = LocalBuf(1024, 2)
  *   BufSamples.ir(buf).poll(0) // 2 * 1024 = 2048
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufChannels$ BufChannels]]
  * @see [[de.sciss.synth.ugen.BufDur$ BufDur]]
  */
object BufSamples extends Reader[BufSamples] {
  /** @param buf              Buffer id.
    */
  def ir(buf: GE): BufSamples = new BufSamples(scalar, buf)
  
  /** @param buf              Buffer id.
    */
  def kr(buf: GE): BufSamples = new BufSamples(control, buf)
  
  def read(in: DataInput): BufSamples = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    new BufSamples(_rate, _buf)
  }
}

/** Returns the current number of allocated samples in the Buffer at the provided
  * index. A sample is not the same as a frame (compare with
  * [[de.sciss.synth.ugen.BufFrames$ BufFrames]] ); a frame includes the samples in
  * each channel of the buffer. Only for a mono buffer are samples the same as
  * frames.
  * {{{
  * samples = frames * numChannels
  * }}}
  * 
  * 
  * @param buf              Buffer id.
  * 
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufChannels$ BufChannels]]
  * @see [[de.sciss.synth.ugen.BufDur$ BufDur]]
  */
final case class BufSamples(rate: Rate, buf: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Returns the number of allocated frames of the buffer at the provided index.
  * 
  * ===Examples===
  * 
  * {{{
  * // frames of local buffer
  * play {
  *   val buf = LocalBuf(1024, 2)
  *   BufFrames.ir(buf).poll(0) // reports 1024
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufSamples$ BufSamples]]
  * @see [[de.sciss.synth.ugen.BufChannels$ BufChannels]]
  * @see [[de.sciss.synth.ugen.BufDur$ BufDur]]
  */
object BufFrames extends Reader[BufFrames] {
  /** @param buf              Buffer id.
    */
  def ir(buf: GE): BufFrames = new BufFrames(scalar, buf)
  
  /** @param buf              Buffer id.
    */
  def kr(buf: GE): BufFrames = new BufFrames(control, buf)
  
  def read(in: DataInput): BufFrames = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    new BufFrames(_rate, _buf)
  }
}

/** Returns the number of allocated frames of the buffer at the provided index.
  * 
  * @param buf              Buffer id.
  * 
  * @see [[de.sciss.synth.ugen.BufSamples$ BufSamples]]
  * @see [[de.sciss.synth.ugen.BufChannels$ BufChannels]]
  * @see [[de.sciss.synth.ugen.BufDur$ BufDur]]
  */
final case class BufFrames(rate: Rate, buf: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Returns the current number of channels of the buffer at the provided index.
  * 
  * ===Examples===
  * 
  * {{{
  * // channels of local buffer
  * play {
  *   val buf = LocalBuf(1024, 2)
  *   BufChannels.ir(buf).poll(0) // reports 2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufSamples$ BufSamples]]
  */
object BufChannels extends Reader[BufChannels] {
  /** @param buf              Buffer id.
    */
  def ir(buf: GE): BufChannels = new BufChannels(scalar, buf)
  
  /** @param buf              Buffer id.
    */
  def kr(buf: GE): BufChannels = new BufChannels(control, buf)
  
  def read(in: DataInput): BufChannels = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    new BufChannels(_rate, _buf)
  }
}

/** Returns the current number of channels of the buffer at the provided index.
  * 
  * @param buf              Buffer id.
  * 
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufSamples$ BufSamples]]
  */
final case class BufChannels(rate: Rate, buf: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Returns the current duration of the buffer at the provided index.
  * 
  * ===Examples===
  * 
  * {{{
  * // duration of local buffer
  * play {
  *   val buf = LocalBuf(SampleRate.ir * 1.5, 2)
  *   BufDur.ir(buf).poll(0) // reports 1.5
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  */
object BufDur extends Reader[BufDur] {
  /** @param buf              Buffer id.
    */
  def ir(buf: GE): BufDur = new BufDur(scalar, buf)
  
  /** @param buf              Buffer id.
    */
  def kr(buf: GE): BufDur = new BufDur(control, buf)
  
  def read(in: DataInput): BufDur = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    new BufDur(_rate, _buf)
  }
}

/** Returns the current duration of the buffer at the provided index.
  * 
  * @param buf              Buffer id.
  * 
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  * @see [[de.sciss.synth.ugen.SampleDur$ SampleDur]]
  */
final case class BufDur(rate: Rate, buf: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen to play back samples from a buffer in memory.
  * 
  * `PlayBuf` provides a kind of high-level interface to sample-playback, whereas
  * `BufRd` represents a kind of lower-level access. While `BufRd` has a
  * random-access-pointer in the form of a phase input, `PlayBuf` advances the phase
  * automatically based on a given playback speed. `PlayBuf` uses cubic
  * interpolation.
  * 
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.DiskIn$ DiskIn]]
  * @see [[de.sciss.synth.ugen.RecordBuf$ RecordBuf]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  */
object PlayBuf extends Reader[PlayBuf] {
  /** @param numChannels      the number of channels that the buffer will be. Since
    *                         this is a constant, a change in number of channels of
    *                         the underlying bus must be reflected by creating
    *                         different SynthDefs. If a buffer identifier is used of a
    *                         buffer that has a different numChannels then specified
    *                         in the PlayBuf, it will fail silently.
    * @param buf              the identifier of the buffer to use
    * @param speed            1 advances the play head by the server's sample rate
    *                         each second. So 2 means doubling speed (and pitch), and
    *                         0.5 means half speed (and half pitch). Negative numbers
    *                         can be used for backwards playback. If the underlying
    *                         buffer represents a sound at a different sample rate,
    *                         the rate should be multiplied by `BufRateScale.kr(buf)`
    *                         to obtain the correct speed.
    * @param trig             a trigger which causes a jump to the given `offset` . A
    *                         trigger occurs when a signal changes from non-positive
    *                         to positive (e.g. <= 0 to > 0).
    * @param offset           sample frame to start playback. This is read when a
    *                         trigger occurs. It may be fractional.
    * @param loop             1 to loop after the play head reaches the buffer end, 0
    *                         to not loop. This can be modulated.
    * @param doneAction       what to do when the play head reaches the buffer end.
    *                         This is only effective when `loop` is zero.
    */
  def kr(numChannels: Int, buf: GE, speed: GE = 1.0f, trig: GE = 1, offset: GE = 0.0f, loop: GE = 1, doneAction: GE = doNothing): PlayBuf = 
    new PlayBuf(control, numChannels, buf, speed, trig, offset, loop, doneAction)
  
  /** @param numChannels      the number of channels that the buffer will be. Since
    *                         this is a constant, a change in number of channels of
    *                         the underlying bus must be reflected by creating
    *                         different SynthDefs. If a buffer identifier is used of a
    *                         buffer that has a different numChannels then specified
    *                         in the PlayBuf, it will fail silently.
    * @param buf              the identifier of the buffer to use
    * @param speed            1 advances the play head by the server's sample rate
    *                         each second. So 2 means doubling speed (and pitch), and
    *                         0.5 means half speed (and half pitch). Negative numbers
    *                         can be used for backwards playback. If the underlying
    *                         buffer represents a sound at a different sample rate,
    *                         the rate should be multiplied by `BufRateScale.kr(buf)`
    *                         to obtain the correct speed.
    * @param trig             a trigger which causes a jump to the given `offset` . A
    *                         trigger occurs when a signal changes from non-positive
    *                         to positive (e.g. <= 0 to > 0).
    * @param offset           sample frame to start playback. This is read when a
    *                         trigger occurs. It may be fractional.
    * @param loop             1 to loop after the play head reaches the buffer end, 0
    *                         to not loop. This can be modulated.
    * @param doneAction       what to do when the play head reaches the buffer end.
    *                         This is only effective when `loop` is zero.
    */
  def ar(numChannels: Int, buf: GE, speed: GE = 1.0f, trig: GE = 1, offset: GE = 0.0f, loop: GE = 1, doneAction: GE = doNothing): PlayBuf = 
    new PlayBuf(audio, numChannels, buf, speed, trig, offset, loop, doneAction)
  
  def read(in: DataInput): PlayBuf = {
    readArity(in, 8)
    val _rate         = readRate(in)
    val _numChannels  = readInt(in)
    val _buf          = readGE(in)
    val _speed        = readGE(in)
    val _trig         = readGE(in)
    val _offset       = readGE(in)
    val _loop         = readGE(in)
    val _doneAction   = readGE(in)
    new PlayBuf(_rate, _numChannels, _buf, _speed, _trig, _offset, _loop, _doneAction)
  }
}

/** A UGen to play back samples from a buffer in memory.
  * 
  * `PlayBuf` provides a kind of high-level interface to sample-playback, whereas
  * `BufRd` represents a kind of lower-level access. While `BufRd` has a
  * random-access-pointer in the form of a phase input, `PlayBuf` advances the phase
  * automatically based on a given playback speed. `PlayBuf` uses cubic
  * interpolation.
  * 
  * @param numChannels      the number of channels that the buffer will be. Since
  *                         this is a constant, a change in number of channels of
  *                         the underlying bus must be reflected by creating
  *                         different SynthDefs. If a buffer identifier is used of a
  *                         buffer that has a different numChannels then specified
  *                         in the PlayBuf, it will fail silently.
  * @param buf              the identifier of the buffer to use
  * @param speed            1 advances the play head by the server's sample rate
  *                         each second. So 2 means doubling speed (and pitch), and
  *                         0.5 means half speed (and half pitch). Negative numbers
  *                         can be used for backwards playback. If the underlying
  *                         buffer represents a sound at a different sample rate,
  *                         the rate should be multiplied by `BufRateScale.kr(buf)`
  *                         to obtain the correct speed.
  * @param trig             a trigger which causes a jump to the given `offset` . A
  *                         trigger occurs when a signal changes from non-positive
  *                         to positive (e.g. <= 0 to > 0).
  * @param offset           sample frame to start playback. This is read when a
  *                         trigger occurs. It may be fractional.
  * @param loop             1 to loop after the play head reaches the buffer end, 0
  *                         to not loop. This can be modulated.
  * @param doneAction       what to do when the play head reaches the buffer end.
  *                         This is only effective when `loop` is zero.
  * 
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.DiskIn$ DiskIn]]
  * @see [[de.sciss.synth.ugen.RecordBuf$ RecordBuf]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  */
final case class PlayBuf(rate: Rate, numChannels: Int, buf: GE, speed: GE = 1.0f, trig: GE = 1, offset: GE = 0.0f, loop: GE = 1, doneAction: GE = doNothing)
  extends UGenSource.MultiOut with HasSideEffect with IsIndividual with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, speed.expand, trig.expand, offset.expand, loop.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(numChannels)(rate), _args, hasSideEffect = true, isIndividual = true)
}

/** Records input into a Buffer. If recLevel is 1.0 and preLevel is 0.0 then the
  * new input overwrites the old data. If they are both 1.0 then the new data is
  * added to the existing data. (Any other settings are also valid.)
  * 
  * ===Examples===
  * 
  * {{{
  * // record and replay
  * // a four second mono buffer
  * val b = Buffer.alloc(s, s.sampleRate.toInt * 4)
  * 
  * // record for four seconds
  * play {
  *   val sig = Formant.ar(XLine.kr(400, 1000, 4), 2000, 800) * 0.125
  *   RecordBuf.ar(sig, b.id, doneAction = freeSelf, loop = 0)
  * }
  * 
  * // play it back
  * play {
  *   PlayBuf.ar(1, b.id, doneAction = freeSelf, loop = 0)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufWr$ BufWr]]
  * @see [[de.sciss.synth.ugen.DiskOut$ DiskOut]]
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  */
object RecordBuf extends Reader[RecordBuf] {
  /** @param in               the signal to record
    * @param buf              the identifier of the buffer to use
    * @param offset           sample frame to begin writing from. This is read when a
    *                         trigger occurs.
    * @param recLevel         value to multiply by input before mixing with existing
    *                         data.
    * @param preLevel         value by which the previous buffer contents is
    *                         multiplied when recording. If this value is zero, the
    *                         buffer contents is completely overwritten. If this value
    *                         is one, the new signal is added to the previous content.
    * @param run              if zero the recording pauses, otherwise it resumes. The
    *                         value of run is only read at control-rate! When the
    *                         recording is paused, the "write-head" remains in its
    *                         current position and does not advance.
    * @param loop             1 to loop after the write head reaches the buffer end,
    *                         0 to not loop. This can be modulated.
    * @param trig             a trigger which causes a jump to the given `offset` . A
    *                         trigger occurs when a signal changes from non-positive
    *                         to positive (e.g. <= 0 to > 0).
    * @param doneAction       what to do when the write head reaches the buffer end.
    *                         This is only effective when `loop` is zero.
    */
  def kr(in: GE, buf: GE, offset: GE = 0, recLevel: GE = 1.0f, preLevel: GE = 0.0f, run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing): RecordBuf = 
    new RecordBuf(control, in, buf, offset, recLevel, preLevel, run, loop, trig, doneAction)
  
  /** @param in               the signal to record
    * @param buf              the identifier of the buffer to use
    * @param offset           sample frame to begin writing from. This is read when a
    *                         trigger occurs.
    * @param recLevel         value to multiply by input before mixing with existing
    *                         data.
    * @param preLevel         value by which the previous buffer contents is
    *                         multiplied when recording. If this value is zero, the
    *                         buffer contents is completely overwritten. If this value
    *                         is one, the new signal is added to the previous content.
    * @param run              if zero the recording pauses, otherwise it resumes. The
    *                         value of run is only read at control-rate! When the
    *                         recording is paused, the "write-head" remains in its
    *                         current position and does not advance.
    * @param loop             1 to loop after the write head reaches the buffer end,
    *                         0 to not loop. This can be modulated.
    * @param trig             a trigger which causes a jump to the given `offset` . A
    *                         trigger occurs when a signal changes from non-positive
    *                         to positive (e.g. <= 0 to > 0).
    * @param doneAction       what to do when the write head reaches the buffer end.
    *                         This is only effective when `loop` is zero.
    */
  def ar(in: GE, buf: GE, offset: GE = 0, recLevel: GE = 1.0f, preLevel: GE = 0.0f, run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing): RecordBuf = 
    new RecordBuf(audio, in, buf, offset, recLevel, preLevel, run, loop, trig, doneAction)
  
  def read(in: DataInput): RecordBuf = {
    readArity(in, 10)
    val _rate       = readRate(in)
    val _in         = readGE(in)
    val _buf        = readGE(in)
    val _offset     = readGE(in)
    val _recLevel   = readGE(in)
    val _preLevel   = readGE(in)
    val _run        = readGE(in)
    val _loop       = readGE(in)
    val _trig       = readGE(in)
    val _doneAction = readGE(in)
    new RecordBuf(_rate, _in, _buf, _offset, _recLevel, _preLevel, _run, _loop, _trig, _doneAction)
  }
}

/** Records input into a Buffer. If recLevel is 1.0 and preLevel is 0.0 then the
  * new input overwrites the old data. If they are both 1.0 then the new data is
  * added to the existing data. (Any other settings are also valid.)
  * 
  * @param in               the signal to record
  * @param buf              the identifier of the buffer to use
  * @param offset           sample frame to begin writing from. This is read when a
  *                         trigger occurs.
  * @param recLevel         value to multiply by input before mixing with existing
  *                         data.
  * @param preLevel         value by which the previous buffer contents is
  *                         multiplied when recording. If this value is zero, the
  *                         buffer contents is completely overwritten. If this value
  *                         is one, the new signal is added to the previous content.
  * @param run              if zero the recording pauses, otherwise it resumes. The
  *                         value of run is only read at control-rate! When the
  *                         recording is paused, the "write-head" remains in its
  *                         current position and does not advance.
  * @param loop             1 to loop after the write head reaches the buffer end,
  *                         0 to not loop. This can be modulated.
  * @param trig             a trigger which causes a jump to the given `offset` . A
  *                         trigger occurs when a signal changes from non-positive
  *                         to positive (e.g. <= 0 to > 0).
  * @param doneAction       what to do when the write head reaches the buffer end.
  *                         This is only effective when `loop` is zero.
  * 
  * @see [[de.sciss.synth.ugen.BufWr$ BufWr]]
  * @see [[de.sciss.synth.ugen.DiskOut$ DiskOut]]
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  */
final case class RecordBuf(rate: Rate, in: GE, buf: GE, offset: GE = 0, recLevel: GE = 1.0f, preLevel: GE = 0.0f, run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, offset.expand, recLevel.expand, preLevel.expand, run.expand, loop.expand, trig.expand, doneAction.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, rate, _args, hasSideEffect = true, isIndividual = true)
}

/** A UGen which reads the content of a buffer, using an index pointer.
  * 
  * Warning: if the supplied `buf` refers to a buffer whose number of channels
  * differs from `numChannels` , the UGen will fail silently.
  * 
  * An alternative to `BufRd` is `PlayBuf` . While `PlayBuf` plays through the
  * buffer by itself, `BufRd` only moves its read point by the index input and
  * therefore has no pitch input. `PlayBuf` uses cubic interpolation, while `BufRd`
  * has variable interpolation. `PlayBuf` can determine the end of the buffer and
  * issue a done-action.
  * 
  * ===Examples===
  * 
  * {{{
  * // Write and read
  * val b = Buffer.alloc(s, numFrames = 32768, numChannels = 1)
  * 
  * // write into the buffer with a BufWr
  * val y = play {
  *   val in = SinOsc.ar(LFNoise1.kr(2).mulAdd(300, 400)) * 0.1
  *   val rate = "rate" kr 1
  *   BufWr.ar(in, b.id, Phasor.ar(0, BufRateScale.kr(b.id) * rate, 0, BufFrames.kr(b.id)))
  *   0.0 // quiet
  * }
  * 
  * // read it with a BufRd
  * val x = play {
  *   val rate = "rate" kr 1
  *   BufRd.ar(1, b.id, Phasor.ar(0, BufRateScale.kr(b.id) * rate, 0, BufFrames.kr(b.id)))
  * }
  * 
  * y.set("rate" -> 0.5) // notice the clicks when the play head overtakes the write head!
  * x.set("rate" -> 0.5)
  * y.set("rate" -> 1.0)
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.ugen.BufWr$ BufWr]]
  * @see [[de.sciss.synth.ugen.Phasor$ Phasor]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  */
object BufRd extends Reader[BufRd] {
  /** @param numChannels      number of channels that the buffer will be. Since this
    *                         is an integer constant, a change in the number of
    *                         channels must be reflected by creating different
    *                         SynthDefs.
    * @param buf              the identifier of the buffer to use
    * @param index            audio rate frame-index into the buffer. Can be
    *                         fractional.
    * @param loop             1 to enable looping, 0 to disable looping. this can be
    *                         modulated.
    * @param interp           1 for no interpolation, 2 for linear, and 4 for cubic
    *                         interpolation. ''(init-time only)''
    */
  def ir(numChannels: Int, buf: GE, index: GE = 0.0f, loop: GE = 1, interp: GE = 2): BufRd = 
    new BufRd(scalar, numChannels, buf, index, loop, interp)
  
  /** @param numChannels      number of channels that the buffer will be. Since this
    *                         is an integer constant, a change in the number of
    *                         channels must be reflected by creating different
    *                         SynthDefs.
    * @param buf              the identifier of the buffer to use
    * @param index            audio rate frame-index into the buffer. Can be
    *                         fractional.
    * @param loop             1 to enable looping, 0 to disable looping. this can be
    *                         modulated.
    * @param interp           1 for no interpolation, 2 for linear, and 4 for cubic
    *                         interpolation. ''(init-time only)''
    */
  def kr(numChannels: Int, buf: GE, index: GE = 0.0f, loop: GE = 1, interp: GE = 2): BufRd = 
    new BufRd(control, numChannels, buf, index, loop, interp)
  
  /** @param numChannels      number of channels that the buffer will be. Since this
    *                         is an integer constant, a change in the number of
    *                         channels must be reflected by creating different
    *                         SynthDefs.
    * @param buf              the identifier of the buffer to use
    * @param index            audio rate frame-index into the buffer. Can be
    *                         fractional.
    * @param loop             1 to enable looping, 0 to disable looping. this can be
    *                         modulated.
    * @param interp           1 for no interpolation, 2 for linear, and 4 for cubic
    *                         interpolation. ''(init-time only)''
    */
  def ar(numChannels: Int, buf: GE, index: GE = 0.0f, loop: GE = 1, interp: GE = 2): BufRd = 
    new BufRd(audio, numChannels, buf, index, loop, interp)
  
  def read(in: DataInput): BufRd = {
    readArity(in, 6)
    val _rate         = readRate(in)
    val _numChannels  = readInt(in)
    val _buf          = readGE(in)
    val _index        = readGE(in)
    val _loop         = readGE(in)
    val _interp       = readGE(in)
    new BufRd(_rate, _numChannels, _buf, _index, _loop, _interp)
  }
}

/** A UGen which reads the content of a buffer, using an index pointer.
  * 
  * Warning: if the supplied `buf` refers to a buffer whose number of channels
  * differs from `numChannels` , the UGen will fail silently.
  * 
  * An alternative to `BufRd` is `PlayBuf` . While `PlayBuf` plays through the
  * buffer by itself, `BufRd` only moves its read point by the index input and
  * therefore has no pitch input. `PlayBuf` uses cubic interpolation, while `BufRd`
  * has variable interpolation. `PlayBuf` can determine the end of the buffer and
  * issue a done-action.
  * 
  * @param numChannels      number of channels that the buffer will be. Since this
  *                         is an integer constant, a change in the number of
  *                         channels must be reflected by creating different
  *                         SynthDefs.
  * @param buf              the identifier of the buffer to use
  * @param index            audio rate frame-index into the buffer. Can be
  *                         fractional.
  * @param loop             1 to enable looping, 0 to disable looping. this can be
  *                         modulated.
  * @param interp           1 for no interpolation, 2 for linear, and 4 for cubic
  *                         interpolation. ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.ugen.BufWr$ BufWr]]
  * @see [[de.sciss.synth.ugen.Phasor$ Phasor]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  */
final case class BufRd(rate: Rate, numChannels: Int, buf: GE, index: GE = 0.0f, loop: GE = 1, interp: GE = 2)
  extends UGenSource.MultiOut with IsIndividual with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, index.expand, loop.expand, interp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.MultiOut(name, rate, Vector.fill(numChannels)(rate), _args1, isIndividual = true)
  }
}

/** A UGen that writes a signal to a buffer, using an index pointer.
  * 
  * Warning: if the supplied `buf` refers to a buffer whose number of channels
  * differs from those of the input signal, the UGen will fail silently.
  * 
  * An alternative to `BufWr` is `RecordBuf` . While `RecordBuf` advances the index
  * into the buffer by itself, `BufWr` only moves its write point by the index
  * input, making it possible to adjust the writing speed or to access the buffer in
  * a non-linear way. `RecordBuf` can determine the end of the buffer and issue a
  * done-action.
  * 
  * ===Examples===
  * 
  * {{{
  * // record and playback
  * // a two second mono buffer
  * val b = Buffer.alloc(s, numFrames = s.sampleRate.toInt * 2)
  * 
  * val y = play {
  *   val sig  = SinOsc.ar(LFNoise1.kr(2).mulAdd(300, 400)) * 0.1
  *   val rate = "rate" kr 1
  *   BufWr.ar(in = sig, buf = b.id, index =
  *     Phasor.ar(speed = BufRateScale.kr(b.id) * rate, lo = 0, hi = BufFrames.kr(b.id)))
  *   0.0 // quiet
  * }
  * 
  * // read it with a BufRd
  * val x = play {
  *   val rate = "rate" kr 1
  *   BufRd.ar(1, buf = b.id, index =
  *     Phasor.ar(speed = BufRateScale.kr(b.id) * rate, lo = 0, hi = BufFrames.kr(b.id)))
  * }
  * 
  * x.set("rate" -> 5)
  * y.set("rate" -> 3)
  * x.set("rate" -> 2)
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.RecordBuf$ RecordBuf]]
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.Phasor$ Phasor]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  */
object BufWr extends Reader[BufWr] {
  /** @param in               input signal to record
    * @param buf              the identifier of the buffer to use
    * @param index            audio rate frame-index into the buffer.
    * @param loop             1 to enable looping, 0 to disable looping. this can be
    *                         modulated.
    */
  def ir(in: GE, buf: GE, index: GE = 0.0f, loop: GE = 1): BufWr = 
    new BufWr(scalar, in, buf, index, loop)
  
  /** @param in               input signal to record
    * @param buf              the identifier of the buffer to use
    * @param index            audio rate frame-index into the buffer.
    * @param loop             1 to enable looping, 0 to disable looping. this can be
    *                         modulated.
    */
  def kr(in: GE, buf: GE, index: GE = 0.0f, loop: GE = 1): BufWr = 
    new BufWr(control, in, buf, index, loop)
  
  /** @param in               input signal to record
    * @param buf              the identifier of the buffer to use
    * @param index            audio rate frame-index into the buffer.
    * @param loop             1 to enable looping, 0 to disable looping. this can be
    *                         modulated.
    */
  def ar(in: GE, buf: GE, index: GE = 0.0f, loop: GE = 1): BufWr = new BufWr(audio, in, buf, index, loop)
  
  def read(in: DataInput): BufWr = {
    readArity(in, 5)
    val _rate   = readRate(in)
    val _in     = readGE(in)
    val _buf    = readGE(in)
    val _index  = readGE(in)
    val _loop   = readGE(in)
    new BufWr(_rate, _in, _buf, _index, _loop)
  }
}

/** A UGen that writes a signal to a buffer, using an index pointer.
  * 
  * Warning: if the supplied `buf` refers to a buffer whose number of channels
  * differs from those of the input signal, the UGen will fail silently.
  * 
  * An alternative to `BufWr` is `RecordBuf` . While `RecordBuf` advances the index
  * into the buffer by itself, `BufWr` only moves its write point by the index
  * input, making it possible to adjust the writing speed or to access the buffer in
  * a non-linear way. `RecordBuf` can determine the end of the buffer and issue a
  * done-action.
  * 
  * @param in               input signal to record
  * @param buf              the identifier of the buffer to use
  * @param index            audio rate frame-index into the buffer.
  * @param loop             1 to enable looping, 0 to disable looping. this can be
  *                         modulated.
  * 
  * @see [[de.sciss.synth.ugen.RecordBuf$ RecordBuf]]
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.Phasor$ Phasor]]
  * @see [[de.sciss.synth.ugen.BufFrames$ BufFrames]]
  * @see [[de.sciss.synth.ugen.BufRateScale$ BufRateScale]]
  */
final case class BufWr(rate: Rate, in: GE, buf: GE, index: GE = 0.0f, loop: GE = 1)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, index.expand, loop.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** An autocorrelation based pitch following UGen. It is more accurate than
  * `ZeroCrossing` , but more also more CPU costly. For most purposes the default
  * settings can be used and only `in` needs to be supplied.
  * 
  * The UGen has two outputs: The first output is the frequency estimate in Hertz,
  * the second output is a toggle `hasFreq` , which tells whether a pitch was found
  * (1) or not (0). If the `clarify` argument is used, `hasFreq` has more fine
  * grained information.
  * 
  * The pitch follower executes periodically at the rate specified by `execFreq` in
  * cps. First it detects whether the input peak to peak amplitude is above the
  * `ampThresh` . If it is not then no pitch estimation is performed, the `hasFreq`
  * output is set to zero and the `freq` output is held at its previous value.
  * Otherwise, the autocorrelation is calculated, and the first peak after the peak
  * around the lag of zero that is above `peakThresh` times the amplitude of the
  * peak at lag zero is reported.
  * 
  * ===Examples===
  * 
  * {{{
  * // pitch-follower resynthesizing with saw tooth
  * play {
  *   // be careful and use headphones!
  *   val in      = Mix(PhysicalIn.ar(0, 2))
  *   val amp     = Amplitude.kr(in, 0.05, 0.05)
  *   val p       = Pitch.kr(in, ampThresh = 0.02, median = 7)
  *   val saw     = Mix(VarSaw.ar(p.freq * Seq(0.5, 1, 2), 0, LFNoise1.kr(0.3,0.1,0.1)) * amp)
  *   Mix.fold(saw, 6) { res =>
  *     AllpassN.ar(res, 0.040, Rand(0, 0.040), Rand(0, 0.040), 2)
  *   }
  * }
  * }}}
  */
object Pitch extends Reader[Pitch] {
  /** @param in               The signal to be analyzed.
    * @param initFreq         The initial value of the `freq` output, until the first
    *                         valid pitch is found. ''(init-time only)''
    * @param minFreq          The minimum frequency in Hertz to be considered for
    *                         reporting. ''(init-time only)''
    * @param maxFreq          The maximum frequency in Hertz to be considered for
    *                         reporting. ''(init-time only)''
    * @param execFreq         The frequency at which the pitch is estimated. This
    *                         will be automatically clipped to be between `minFreq`
    *                         and `maxFreq` . ''(init-time only)''
    * @param binsPerOct       A value which guides the search for the peak frequency
    *                         in the first coarse step. Its setting does *not* affect
    *                         the final pitch resolution; setting it larger will cause
    *                         the coarse search to take longer, and setting it smaller
    *                         will cause the fine search to take longer. ''(init-time
    *                         only)''
    * @param median           This specifies the length of a median filter applied to
    *                         the frequency output estimation. With the default value
    *                         of `1` the filter is defeated. Median filtering can help
    *                         eliminating single spikes and jitter. This will however
    *                         add latency to the output. ''(init-time only)''
    * @param ampThresh        The minimum amplitude threshold above which the pitch
    *                         follower operates. An input signal below this threshold
    *                         is not analyzed. ''(init-time only)''
    * @param peakThresh       This is a threshold used to find the first peak in the
    *                         autocorrelation signal which gives the reported
    *                         frequency. It is a factor of the energy of the signal
    *                         (autocorrelation coefficient at zero). Set this value
    *                         higher (e.g. to `1` ) to eliminate false frequencies
    *                         corresponding to overtones. ''(init-time only)''
    * @param downSample       An integer factor by which the input signal is down
    *                         sampled to reduce CPU overhead. This will also reduce
    *                         the pitch resolution. The default value of `1` means
    *                         that the input signal is not down sampled. ''(init-time
    *                         only)''
    * @param clarity          If the `clarity` argument is greater than zero (it is
    *                         zero by default) then the `hasFreq` output is given
    *                         additional detail. Rather than simply being 1 when a
    *                         pitch is detected, it is a "clarity" measure in the
    *                         range between zero and one. (Technically, it's the
    *                         height of the autocorrelation peak normalised by the
    *                         height of the zero-lag peak.) It therefore gives a kind
    *                         of measure of "purity" of the pitched signal.
    *                         ''(init-time only)''
    */
  def kr(in: GE, initFreq: GE = 440.0f, minFreq: GE = 60.0f, maxFreq: GE = 4000.0f, execFreq: GE = 100.0f, binsPerOct: GE = 16, median: GE = 1, ampThresh: GE = 0.01f, peakThresh: GE = 0.5f, downSample: GE = 1, clarity: GE = 0): Pitch = 
    new Pitch(control, in, initFreq, minFreq, maxFreq, execFreq, binsPerOct, median, ampThresh, peakThresh, downSample, clarity)
  
  def read(in: DataInput): Pitch = {
    readArity(in, 12)
    val _rate       = readRate(in)
    val _in         = readGE(in)
    val _initFreq   = readGE(in)
    val _minFreq    = readGE(in)
    val _maxFreq    = readGE(in)
    val _execFreq   = readGE(in)
    val _binsPerOct = readGE(in)
    val _median     = readGE(in)
    val _ampThresh  = readGE(in)
    val _peakThresh = readGE(in)
    val _downSample = readGE(in)
    val _clarity    = readGE(in)
    new Pitch(_rate, _in, _initFreq, _minFreq, _maxFreq, _execFreq, _binsPerOct, _median, _ampThresh, _peakThresh, _downSample, _clarity)
  }
}

/** An autocorrelation based pitch following UGen. It is more accurate than
  * `ZeroCrossing` , but more also more CPU costly. For most purposes the default
  * settings can be used and only `in` needs to be supplied.
  * 
  * The UGen has two outputs: The first output is the frequency estimate in Hertz,
  * the second output is a toggle `hasFreq` , which tells whether a pitch was found
  * (1) or not (0). If the `clarify` argument is used, `hasFreq` has more fine
  * grained information.
  * 
  * The pitch follower executes periodically at the rate specified by `execFreq` in
  * cps. First it detects whether the input peak to peak amplitude is above the
  * `ampThresh` . If it is not then no pitch estimation is performed, the `hasFreq`
  * output is set to zero and the `freq` output is held at its previous value.
  * Otherwise, the autocorrelation is calculated, and the first peak after the peak
  * around the lag of zero that is above `peakThresh` times the amplitude of the
  * peak at lag zero is reported.
  * 
  * @param in               The signal to be analyzed.
  * @param initFreq         The initial value of the `freq` output, until the first
  *                         valid pitch is found. ''(init-time only)''
  * @param minFreq          The minimum frequency in Hertz to be considered for
  *                         reporting. ''(init-time only)''
  * @param maxFreq          The maximum frequency in Hertz to be considered for
  *                         reporting. ''(init-time only)''
  * @param execFreq         The frequency at which the pitch is estimated. This
  *                         will be automatically clipped to be between `minFreq`
  *                         and `maxFreq` . ''(init-time only)''
  * @param binsPerOct       A value which guides the search for the peak frequency
  *                         in the first coarse step. Its setting does *not* affect
  *                         the final pitch resolution; setting it larger will cause
  *                         the coarse search to take longer, and setting it smaller
  *                         will cause the fine search to take longer. ''(init-time
  *                         only)''
  * @param median           This specifies the length of a median filter applied to
  *                         the frequency output estimation. With the default value
  *                         of `1` the filter is defeated. Median filtering can help
  *                         eliminating single spikes and jitter. This will however
  *                         add latency to the output. ''(init-time only)''
  * @param ampThresh        The minimum amplitude threshold above which the pitch
  *                         follower operates. An input signal below this threshold
  *                         is not analyzed. ''(init-time only)''
  * @param peakThresh       This is a threshold used to find the first peak in the
  *                         autocorrelation signal which gives the reported
  *                         frequency. It is a factor of the energy of the signal
  *                         (autocorrelation coefficient at zero). Set this value
  *                         higher (e.g. to `1` ) to eliminate false frequencies
  *                         corresponding to overtones. ''(init-time only)''
  * @param downSample       An integer factor by which the input signal is down
  *                         sampled to reduce CPU overhead. This will also reduce
  *                         the pitch resolution. The default value of `1` means
  *                         that the input signal is not down sampled. ''(init-time
  *                         only)''
  * @param clarity          If the `clarity` argument is greater than zero (it is
  *                         zero by default) then the `hasFreq` output is given
  *                         additional detail. Rather than simply being 1 when a
  *                         pitch is detected, it is a "clarity" measure in the
  *                         range between zero and one. (Technically, it's the
  *                         height of the autocorrelation peak normalised by the
  *                         height of the zero-lag peak.) It therefore gives a kind
  *                         of measure of "purity" of the pitched signal.
  *                         ''(init-time only)''
  */
final case class Pitch(rate: Rate, in: GE, initFreq: GE = 440.0f, minFreq: GE = 60.0f, maxFreq: GE = 4000.0f, execFreq: GE = 100.0f, binsPerOct: GE = 16, median: GE = 1, ampThresh: GE = 0.01f, peakThresh: GE = 0.5f, downSample: GE = 1, clarity: GE = 0)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, initFreq.expand, minFreq.expand, maxFreq.expand, execFreq.expand, binsPerOct.expand, median.expand, ampThresh.expand, peakThresh.expand, downSample.expand, clarity.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args)
  
  def freq: GE = ChannelProxy(this, 0)
  
  def hasFreq: GE = ChannelProxy(this, 1)
}

/** Simple delay line with no interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * ===Examples===
  * 
  * {{{
  * // Random white-noise decay
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo, 1)
  * 
  * // Dust randomly triggers Decay to create an exponential
  * // decay envelope for the WhiteNoise input source.
  * // We apply a slight filter to the delayed signal
  * // so it is easier to distinguish
  * play {
  *   val z = Decay.ar(Dust.ar(1) * 0.5, 0.3) * WhiteNoise.ar
  *   LPF.ar(BufDelayN.ar(b.id, z, 0.2), 8000) + z  // input is mixed with delay
  * }
  * 
  * b.free()  // do this after the synth has ended
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufDelayL$ BufDelayL]]
  * @see [[de.sciss.synth.ugen.BufDelayC$ BufDelayC]]
  */
object BufDelayN extends Reader[BufDelayN] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    */
  def kr(buf: GE, in: GE, delayTime: GE = 0.2f): BufDelayN = new BufDelayN(control, buf, in, delayTime)
  
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f): BufDelayN = new BufDelayN(audio, buf, in, delayTime)
  
  def read(in: DataInput): BufDelayN = {
    readArity(in, 4)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    new BufDelayN(_rate, _buf, _in, _delayTime)
  }
}

/** Simple delay line with no interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.BufDelayL$ BufDelayL]]
  * @see [[de.sciss.synth.ugen.BufDelayC$ BufDelayC]]
  */
final case class BufDelayN(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** Simple delay line with linear interpolation which uses a buffer for its
  * internal memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @see [[de.sciss.synth.ugen.BufDelayN$ BufDelayN]]
  * @see [[de.sciss.synth.ugen.BufDelayC$ BufDelayC]]
  */
object BufDelayL extends Reader[BufDelayL] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    */
  def kr(buf: GE, in: GE, delayTime: GE = 0.2f): BufDelayL = new BufDelayL(control, buf, in, delayTime)
  
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f): BufDelayL = new BufDelayL(audio, buf, in, delayTime)
  
  def read(in: DataInput): BufDelayL = {
    readArity(in, 4)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    new BufDelayL(_rate, _buf, _in, _delayTime)
  }
}

/** Simple delay line with linear interpolation which uses a buffer for its
  * internal memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.BufDelayN$ BufDelayN]]
  * @see [[de.sciss.synth.ugen.BufDelayC$ BufDelayC]]
  */
final case class BufDelayL(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** Simple delay line with cubic interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @see [[de.sciss.synth.ugen.BufDelayN$ BufDelayN]]
  * @see [[de.sciss.synth.ugen.BufDelayL$ BufDelayL]]
  */
object BufDelayC extends Reader[BufDelayC] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    */
  def kr(buf: GE, in: GE, delayTime: GE = 0.2f): BufDelayC = new BufDelayC(control, buf, in, delayTime)
  
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f): BufDelayC = new BufDelayC(audio, buf, in, delayTime)
  
  def read(in: DataInput): BufDelayC = {
    readArity(in, 4)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    new BufDelayC(_rate, _buf, _in, _delayTime)
  }
}

/** Simple delay line with cubic interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.BufDelayN$ BufDelayN]]
  * @see [[de.sciss.synth.ugen.BufDelayL$ BufDelayL]]
  */
final case class BufDelayC(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** Comb delay line with no interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * ===Examples===
  * 
  * {{{
  * // Compare interpolation
  * // These examples compare the variants, so that you can hear the difference in interpolation
  * 
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.01 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // Comb used as a resonator. The resonant fundamental is equal to
  * // reciprocal of the delay time.
  * play { BufCombN.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * play { BufCombL.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * play { BufCombC.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * // with negative feedback
  * play { BufCombN.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * play { BufCombL.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * play { BufCombC.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * b.free()   // do this after the synths have ended
  * }}}
  * {{{
  * // Used as an echo
  * val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * play { BufCombN.ar(b.id, Decay.ar(Dust.ar(1) * 0.5, 0.2) * WhiteNoise.ar, 0.2, 3) }
  * 
  * b.free()   // do this after the synth has ended
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufCombL$ BufCombL]]
  * @see [[de.sciss.synth.ugen.BufCombC$ BufCombC]]
  */
object BufCombN extends Reader[BufCombN] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f): BufCombN = 
    new BufCombN(audio, buf, in, delayTime, decayTime)
  
  def read(in: DataInput): BufCombN = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    val _decayTime  = readGE(in)
    new BufCombN(_rate, _buf, _in, _delayTime, _decayTime)
  }
}

/** Comb delay line with no interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.BufCombL$ BufCombL]]
  * @see [[de.sciss.synth.ugen.BufCombC$ BufCombC]]
  */
final case class BufCombN(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** Comb delay line with linear interpolation which uses a buffer for its internal
  * memory.
  * 
  * ===Examples===
  * 
  * {{{
  * // Compare interpolation
  * // These examples compare the variants, so that you can hear the difference in interpolation
  * 
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.01 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // Comb used as a resonator. The resonant fundamental is equal to
  * // reciprocal of the delay time.
  * play { BufCombN.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * play { BufCombL.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * play { BufCombC.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * // with negative feedback
  * play { BufCombN.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * play { BufCombL.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * play { BufCombC.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * b.free()   // do this after the synths have ended
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufCombN$ BufCombN]]
  * @see [[de.sciss.synth.ugen.BufCombC$ BufCombC]]
  */
object BufCombL extends Reader[BufCombL] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower. '''Warning''': For reasons of efficiency,
    *                         the effective buffer size is the allocated size rounded
    *                         down to the next power of two. For example, if 44100
    *                         samples are allocated, the maximum delay would be 32768
    *                         samples. Also note that the buffer must be monophonic.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f): BufCombL = 
    new BufCombL(audio, buf, in, delayTime, decayTime)
  
  def read(in: DataInput): BufCombL = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    val _decayTime  = readGE(in)
    new BufCombL(_rate, _buf, _in, _delayTime, _decayTime)
  }
}

/** Comb delay line with linear interpolation which uses a buffer for its internal
  * memory.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower. '''Warning''': For reasons of efficiency,
  *                         the effective buffer size is the allocated size rounded
  *                         down to the next power of two. For example, if 44100
  *                         samples are allocated, the maximum delay would be 32768
  *                         samples. Also note that the buffer must be monophonic.
  * 
  * @see [[de.sciss.synth.ugen.BufCombN$ BufCombN]]
  * @see [[de.sciss.synth.ugen.BufCombC$ BufCombC]]
  */
final case class BufCombL(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** Comb delay line with cubic interpolation which uses a buffer for its internal
  * memory.
  * 
  * ===Examples===
  * 
  * {{{
  * // Compare interpolation
  * // These examples compare the variants, so that you can hear the difference in interpolation
  * 
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.01 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // Comb used as a resonator. The resonant fundamental is equal to
  * // reciprocal of the delay time.
  * play { BufCombN.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * play { BufCombL.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * play { BufCombC.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * // with negative feedback
  * play { BufCombN.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * play { BufCombL.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * play { BufCombC.ar(b.id, WhiteNoise.ar(0.01), XLine.kr(0.0001, 0.01, 20), -0.2) }
  * 
  * b.free()   // do this after the synths have ended
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufCombN$ BufCombN]]
  * @see [[de.sciss.synth.ugen.BufCombL$ BufCombL]]
  */
object BufCombC extends Reader[BufCombC] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower. '''Warning''': For reasons of efficiency,
    *                         the effective buffer size is the allocated size rounded
    *                         down to the next power of two. For example, if 44100
    *                         samples are allocated, the maximum delay would be 32768
    *                         samples. Also note that the buffer must be monophonic.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f): BufCombC = 
    new BufCombC(audio, buf, in, delayTime, decayTime)
  
  def read(in: DataInput): BufCombC = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    val _decayTime  = readGE(in)
    new BufCombC(_rate, _buf, _in, _delayTime, _decayTime)
  }
}

/** Comb delay line with cubic interpolation which uses a buffer for its internal
  * memory.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower. '''Warning''': For reasons of efficiency,
  *                         the effective buffer size is the allocated size rounded
  *                         down to the next power of two. For example, if 44100
  *                         samples are allocated, the maximum delay would be 32768
  *                         samples. Also note that the buffer must be monophonic.
  * 
  * @see [[de.sciss.synth.ugen.BufCombN$ BufCombN]]
  * @see [[de.sciss.synth.ugen.BufCombL$ BufCombL]]
  */
final case class BufCombC(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** All pass delay line with no interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * ===Examples===
  * 
  * {{{
  * // Compare interpolation types
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // Since the allpass delay has no audible effect as a resonator on
  * // steady state sound ...
  * play { BufAllpassC.ar(b.id, WhiteNoise.ar(0.1), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * // ...these examples add the input to the effected sound and compare variants so that you can hear
  * // the effect of the phase comb:
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassN.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassL.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassC.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * b.free()  // after synths have been stopped
  * }}}
  * {{{
  * // Used as echo
  * val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // doesn't really sound different than Comb,
  * // but it outputs the input signal immediately (inverted) and the echoes
  * // are lower in amplitude.
  * play { BufAllpassN.ar(b.id, Decay.ar(Dust.ar(1) * 0.5, 0.2) * WhiteNoise.ar, 0.2, 3) }
  * 
  * b.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufAllpassL$ BufAllpassL]]
  * @see [[de.sciss.synth.ugen.BufAllpassC$ BufAllpassC]]
  */
object BufAllpassN extends Reader[BufAllpassN] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f): BufAllpassN = 
    new BufAllpassN(audio, buf, in, delayTime, decayTime)
  
  def read(in: DataInput): BufAllpassN = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    val _decayTime  = readGE(in)
    new BufAllpassN(_rate, _buf, _in, _delayTime, _decayTime)
  }
}

/** All pass delay line with no interpolation which uses a buffer for its internal
  * memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.BufAllpassL$ BufAllpassL]]
  * @see [[de.sciss.synth.ugen.BufAllpassC$ BufAllpassC]]
  */
final case class BufAllpassN(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** All pass delay line with linear interpolation which uses a buffer for its
  * internal memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * ===Examples===
  * 
  * {{{
  * // Compare interpolation types
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // Since the allpass delay has no audible effect as a resonator on
  * // steady state sound ...
  * play { BufAllpassC.ar(b.id, WhiteNoise.ar(0.1), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * // ...these examples add the input to the effected sound and compare variants so that you can hear
  * // the effect of the phase comb:
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassN.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassL.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassC.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * b.free()  // after synths have been stopped
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufAllpassN$ BufAllpassN]]
  * @see [[de.sciss.synth.ugen.BufAllpassC$ BufAllpassC]]
  */
object BufAllpassL extends Reader[BufAllpassL] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f): BufAllpassL = 
    new BufAllpassL(audio, buf, in, delayTime, decayTime)
  
  def read(in: DataInput): BufAllpassL = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    val _decayTime  = readGE(in)
    new BufAllpassL(_rate, _buf, _in, _delayTime, _decayTime)
  }
}

/** All pass delay line with linear interpolation which uses a buffer for its
  * internal memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.BufAllpassN$ BufAllpassN]]
  * @see [[de.sciss.synth.ugen.BufAllpassC$ BufAllpassC]]
  */
final case class BufAllpassL(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** All pass delay line with cubic interpolation which uses a buffer for its
  * internal memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * ===Examples===
  * 
  * {{{
  * // Compare interpolation types
  * // allocate buffer
  * val b = Buffer.alloc(s, (0.2 * s.sampleRate).toInt.nextPowerOfTwo)
  * 
  * // Since the allpass delay has no audible effect as a resonator on
  * // steady state sound ...
  * play { BufAllpassC.ar(b.id, WhiteNoise.ar(0.1), XLine.kr(0.0001, 0.01, 20), 0.2) }
  * 
  * // ...these examples add the input to the effected sound and compare variants so that you can hear
  * // the effect of the phase comb:
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassN.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassL.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * play {
  *   val z = WhiteNoise.ar(0.2)
  *   z + BufAllpassC.ar(b.id, z, XLine.kr(0.0001, 0.01, 20), 0.2)
  * }
  * 
  * b.free()  // after synths have been stopped
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BufAllpassN$ BufAllpassN]]
  * @see [[de.sciss.synth.ugen.BufAllpassL$ BufAllpassL]]
  */
object BufAllpassC extends Reader[BufAllpassC] {
  /** @param buf              Buffer id.
    * @param in               The input signal.
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f): BufAllpassC = 
    new BufAllpassC(audio, buf, in, delayTime, decayTime)
  
  def read(in: DataInput): BufAllpassC = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _delayTime  = readGE(in)
    val _decayTime  = readGE(in)
    new BufAllpassC(_rate, _buf, _in, _delayTime, _decayTime)
  }
}

/** All pass delay line with cubic interpolation which uses a buffer for its
  * internal memory.
  * 
  * '''Warning''': For reasons of efficiency, the effective buffer size is the
  * allocated size rounded down to the next power of two. For example, if 44100
  * samples are allocated, the maximum delay would be 32768 samples. Also note that
  * the buffer must be monophonic.
  * 
  * @param buf              Buffer id.
  * @param in               The input signal.
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.BufAllpassN$ BufAllpassN]]
  * @see [[de.sciss.synth.ugen.BufAllpassL$ BufAllpassL]]
  */
final case class BufAllpassC(rate: Rate, buf: GE, in: GE, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true, isIndividual = true)
  }
}

/** Simple delay line with no interpolation. The initial buffer contents is zero.
  * 
  * ===Examples===
  * 
  * {{{
  * // Delayed random pulses
  * play {
  *   // Dust randomly triggers Decay to create an exponential
  *   // decay envelope for the WhiteNoise input source
  *   val z = Decay.ar(Dust.ar(2) * 0.5, 0.3) * WhiteNoise.ar
  *   DelayN.ar(z, 0.2, 0.2) + z  // input is mixed with delay via the add input
  * }
  * }}}
  * {{{
  * // Recursive application
  * play {
  *   val z = Decay2.ar(Dust.ar(1) * 0.5, 0.01, 0.1) * Saw.ar(Seq(100, 101)) * 0.5
  *   (z /: (0 until 5)) { (zi, i) =>
  *     DelayN.ar(RLPF.ar(zi, Rand(100, 3000), 0.03), 1, 1.0 / (2 << i)) + zi * 0.5
  *   }
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.DelayL$ DelayL]]
  * @see [[de.sciss.synth.ugen.DelayC$ DelayC]]
  */
object DelayN extends Reader[DelayN] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f): DelayN = 
    new DelayN(control, in, maxDelayTime, delayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f): DelayN = 
    new DelayN(audio, in, maxDelayTime, delayTime)
  
  def read(in: DataInput): DelayN = {
    readArity(in, 4)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    new DelayN(_rate, _in, _maxDelayTime, _delayTime)
  }
}

/** Simple delay line with no interpolation. The initial buffer contents is zero.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.DelayL$ DelayL]]
  * @see [[de.sciss.synth.ugen.DelayC$ DelayC]]
  */
final case class DelayN(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** Simple delay line with linear interpolation.
  * 
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  * @see [[de.sciss.synth.ugen.DelayC$ DelayC]]
  */
object DelayL extends Reader[DelayL] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f): DelayL = 
    new DelayL(control, in, maxDelayTime, delayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f): DelayL = 
    new DelayL(audio, in, maxDelayTime, delayTime)
  
  def read(in: DataInput): DelayL = {
    readArity(in, 4)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    new DelayL(_rate, _in, _maxDelayTime, _delayTime)
  }
}

/** Simple delay line with linear interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  * @see [[de.sciss.synth.ugen.DelayC$ DelayC]]
  */
final case class DelayL(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** Simple delay line with cubic interpolation.
  * 
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  * @see [[de.sciss.synth.ugen.DelayL$ DelayL]]
  */
object DelayC extends Reader[DelayC] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f): DelayC = 
    new DelayC(control, in, maxDelayTime, delayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f): DelayC = 
    new DelayC(audio, in, maxDelayTime, delayTime)
  
  def read(in: DataInput): DelayC = {
    readArity(in, 4)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    new DelayC(_rate, _in, _maxDelayTime, _delayTime)
  }
}

/** Simple delay line with cubic interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  * @see [[de.sciss.synth.ugen.DelayL$ DelayL]]
  */
final case class DelayC(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** Comb delay line with no interpolation.
  * 
  * @see [[de.sciss.synth.ugen.CombL$ CombL]]
  * @see [[de.sciss.synth.ugen.CombC$ CombC]]
  */
object CombN extends Reader[CombN] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): CombN = 
    new CombN(control, in, maxDelayTime, delayTime, decayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): CombN = 
    new CombN(audio, in, maxDelayTime, delayTime, decayTime)
  
  def read(in: DataInput): CombN = {
    readArity(in, 5)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    new CombN(_rate, _in, _maxDelayTime, _delayTime, _decayTime)
  }
}

/** Comb delay line with no interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.CombL$ CombL]]
  * @see [[de.sciss.synth.ugen.CombC$ CombC]]
  */
final case class CombN(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** Comb delay line with linear interpolation.
  * 
  * @see [[de.sciss.synth.ugen.CombN$ CombN]]
  * @see [[de.sciss.synth.ugen.CombC$ CombC]]
  */
object CombL extends Reader[CombL] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): CombL = 
    new CombL(control, in, maxDelayTime, delayTime, decayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): CombL = 
    new CombL(audio, in, maxDelayTime, delayTime, decayTime)
  
  def read(in: DataInput): CombL = {
    readArity(in, 5)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    new CombL(_rate, _in, _maxDelayTime, _delayTime, _decayTime)
  }
}

/** Comb delay line with linear interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.CombN$ CombN]]
  * @see [[de.sciss.synth.ugen.CombC$ CombC]]
  */
final case class CombL(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** Comb delay line with cubic interpolation.
  * 
  * @see [[de.sciss.synth.ugen.CombN$ CombN]]
  * @see [[de.sciss.synth.ugen.CombL$ CombL]]
  */
object CombC extends Reader[CombC] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): CombC = 
    new CombC(control, in, maxDelayTime, delayTime, decayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): CombC = 
    new CombC(audio, in, maxDelayTime, delayTime, decayTime)
  
  def read(in: DataInput): CombC = {
    readArity(in, 5)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    new CombC(_rate, _in, _maxDelayTime, _delayTime, _decayTime)
  }
}

/** Comb delay line with cubic interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.CombN$ CombN]]
  * @see [[de.sciss.synth.ugen.CombL$ CombL]]
  */
final case class CombC(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** All pass delay line with no interpolation.
  * 
  * @see [[de.sciss.synth.ugen.AllpassL$ AllpassL]]
  * @see [[de.sciss.synth.ugen.AllpassC$ AllpassC]]
  */
object AllpassN extends Reader[AllpassN] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): AllpassN = 
    new AllpassN(control, in, maxDelayTime, delayTime, decayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): AllpassN = 
    new AllpassN(audio, in, maxDelayTime, delayTime, decayTime)
  
  def read(in: DataInput): AllpassN = {
    readArity(in, 5)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    new AllpassN(_rate, _in, _maxDelayTime, _delayTime, _decayTime)
  }
}

/** All pass delay line with no interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.AllpassL$ AllpassL]]
  * @see [[de.sciss.synth.ugen.AllpassC$ AllpassC]]
  */
final case class AllpassN(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** All pass delay line with linear interpolation.
  * 
  * @see [[de.sciss.synth.ugen.AllpassN$ AllpassN]]
  * @see [[de.sciss.synth.ugen.AllpassC$ AllpassC]]
  */
object AllpassL extends Reader[AllpassL] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): AllpassL = 
    new AllpassL(control, in, maxDelayTime, delayTime, decayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): AllpassL = 
    new AllpassL(audio, in, maxDelayTime, delayTime, decayTime)
  
  def read(in: DataInput): AllpassL = {
    readArity(in, 5)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    new AllpassL(_rate, _in, _maxDelayTime, _delayTime, _decayTime)
  }
}

/** All pass delay line with linear interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.AllpassN$ AllpassN]]
  * @see [[de.sciss.synth.ugen.AllpassC$ AllpassC]]
  */
final case class AllpassL(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** All pass delay line with cubic interpolation.
  * 
  * @see [[de.sciss.synth.ugen.AllpassN$ AllpassN]]
  * @see [[de.sciss.synth.ugen.AllpassL$ AllpassL]]
  */
object AllpassC extends Reader[AllpassC] {
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def kr(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): AllpassC = 
    new AllpassC(control, in, maxDelayTime, delayTime, decayTime)
  
  /** @param in               The input signal.
    * @param maxDelayTime     The maximum delay time in seconds. used to initialize
    *                         the delay buffer size. ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. If this
    *                         time is negative then the feedback coefficient will be
    *                         negative, thus emphasizing only odd harmonics at an
    *                         octave lower.
    */
  def ar(in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f): AllpassC = 
    new AllpassC(audio, in, maxDelayTime, delayTime, decayTime)
  
  def read(in: DataInput): AllpassC = {
    readArity(in, 5)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    new AllpassC(_rate, _in, _maxDelayTime, _delayTime, _decayTime)
  }
}

/** All pass delay line with cubic interpolation.
  * 
  * @param in               The input signal.
  * @param maxDelayTime     The maximum delay time in seconds. used to initialize
  *                         the delay buffer size. ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. If this
  *                         time is negative then the feedback coefficient will be
  *                         negative, thus emphasizing only odd harmonics at an
  *                         octave lower.
  * 
  * @see [[de.sciss.synth.ugen.AllpassN$ AllpassN]]
  * @see [[de.sciss.synth.ugen.AllpassL$ AllpassL]]
  */
final case class AllpassC(rate: Rate, in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A time domain granular pitch shifter. Grains have a triangular amplitude
  * envelope and an overlap of 4:1.
  */
object PitchShift extends Reader[PitchShift] {
  /** @param in               The input signal.
    * @param winSize          The size of the grain window in seconds. ''(init-time
    *                         only)''
    * @param pitchRatio       The ratio of the pitch shift. Must be from 0 to 4.
    * @param pitchDispersion  The maximum random deviation of the pitch from the
    *                         pitchRatio.
    * @param timeDispersion   A random offset of from zero to timeDispersion seconds
    *                         is added to the delay of each grain. Use of some
    *                         dispersion can alleviate a hard comb filter effect due
    *                         to uniform grain placement. It can also be an effect in
    *                         itself. timeDispersion can be no larger than windowSize.
    */
  def ar(in: GE, winSize: GE = 0.2f, pitchRatio: GE = 1.0f, pitchDispersion: GE = 0.0f, timeDispersion: GE = 0.0f): PitchShift = 
    new PitchShift(in, winSize, pitchRatio, pitchDispersion, timeDispersion)
  
  def read(in: DataInput): PitchShift = {
    readArity(in, 5)
    val _in               = readGE(in)
    val _winSize          = readGE(in)
    val _pitchRatio       = readGE(in)
    val _pitchDispersion  = readGE(in)
    val _timeDispersion   = readGE(in)
    new PitchShift(_in, _winSize, _pitchRatio, _pitchDispersion, _timeDispersion)
  }
}

/** A time domain granular pitch shifter. Grains have a triangular amplitude
  * envelope and an overlap of 4:1.
  * 
  * @param in               The input signal.
  * @param winSize          The size of the grain window in seconds. ''(init-time
  *                         only)''
  * @param pitchRatio       The ratio of the pitch shift. Must be from 0 to 4.
  * @param pitchDispersion  The maximum random deviation of the pitch from the
  *                         pitchRatio.
  * @param timeDispersion   A random offset of from zero to timeDispersion seconds
  *                         is added to the delay of each grain. Use of some
  *                         dispersion can alleviate a hard comb filter effect due
  *                         to uniform grain placement. It can also be an effect in
  *                         itself. timeDispersion can be no larger than windowSize.
  */
final case class PitchShift(in: GE, winSize: GE = 0.2f, pitchRatio: GE = 1.0f, pitchDispersion: GE = 0.0f, timeDispersion: GE = 0.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, winSize.expand, pitchRatio.expand, pitchDispersion.expand, timeDispersion.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, audio, _args)
}

/** Triggers generate grains from a buffer. Each grain has a Hanning envelope
  * {{{
  * (sin2(x) for x from 0 to pi)
  * }}}
  *  and is panned between two channels of multiple outputs.
  * '''Warning''': Due to a bug (SC 3.6.6), this UGen does not work with `LocalBuf`
  * but requires a regular buffer.
  * 
  * @see [[de.sciss.synth.ugen.GrainBuf$ GrainBuf]]
  */
object TGrains extends Reader[TGrains] {
  /** @param numChannels      Number of output channels.
    * @param trig             At each trigger, the following arguments are sampled
    *                         and used as the arguments of a new grain. A trigger
    *                         occurs when a signal changes from non-positive to
    *                         positive value. If the trigger is audio rate then the
    *                         grains will start with sample accuracy.
    * @param buf              The buffer index. It must be a one channel (mono)
    *                         buffer.
    * @param speed            1.0 is normal, 2.0 is one octave up, 0.5 is one octave
    *                         down -1.0 is backwards normal rate etc.
    * @param centerPos        The position in the buffer in seconds at which the
    *                         grain envelope will reach maximum amplitude.
    * @param dur              Duration of the grain in seconds.
    * @param pan              A value from -1 to 1. Determines where to pan the
    *                         output in the same manner as
    *                         [[de.sciss.synth.ugen.PanAz$ PanAz]].
    * @param amp              Amplitude of the grain.
    * @param interp           1, 2, or 4. Determines whether the grain uses (1) no
    *                         interpolation, (2) linear interpolation, or (4) cubic
    *                         interpolation.
    */
  def ar(numChannels: Int, trig: GE, buf: GE, speed: GE = 1.0f, centerPos: GE = 0.0f, dur: GE = 0.1f, pan: GE = 0.0f, amp: GE = 0.1f, interp: GE = 4): TGrains = 
    new TGrains(numChannels, trig, buf, speed, centerPos, dur, pan, amp, interp)
  
  def read(in: DataInput): TGrains = {
    readArity(in, 9)
    val _numChannels  = readInt(in)
    val _trig         = readGE(in)
    val _buf          = readGE(in)
    val _speed        = readGE(in)
    val _centerPos    = readGE(in)
    val _dur          = readGE(in)
    val _pan          = readGE(in)
    val _amp          = readGE(in)
    val _interp       = readGE(in)
    new TGrains(_numChannels, _trig, _buf, _speed, _centerPos, _dur, _pan, _amp, _interp)
  }
}

/** Triggers generate grains from a buffer. Each grain has a Hanning envelope
  * {{{
  * (sin2(x) for x from 0 to pi)
  * }}}
  *  and is panned between two channels of multiple outputs.
  * '''Warning''': Due to a bug (SC 3.6.6), this UGen does not work with `LocalBuf`
  * but requires a regular buffer.
  * 
  * @param numChannels      Number of output channels.
  * @param trig             At each trigger, the following arguments are sampled
  *                         and used as the arguments of a new grain. A trigger
  *                         occurs when a signal changes from non-positive to
  *                         positive value. If the trigger is audio rate then the
  *                         grains will start with sample accuracy.
  * @param buf              The buffer index. It must be a one channel (mono)
  *                         buffer.
  * @param speed            1.0 is normal, 2.0 is one octave up, 0.5 is one octave
  *                         down -1.0 is backwards normal rate etc.
  * @param centerPos        The position in the buffer in seconds at which the
  *                         grain envelope will reach maximum amplitude.
  * @param dur              Duration of the grain in seconds.
  * @param pan              A value from -1 to 1. Determines where to pan the
  *                         output in the same manner as
  *                         [[de.sciss.synth.ugen.PanAz$ PanAz]].
  * @param amp              Amplitude of the grain.
  * @param interp           1, 2, or 4. Determines whether the grain uses (1) no
  *                         interpolation, (2) linear interpolation, or (4) cubic
  *                         interpolation.
  * 
  * @see [[de.sciss.synth.ugen.GrainBuf$ GrainBuf]]
  */
final case class TGrains(numChannels: Int, trig: GE, buf: GE, speed: GE = 1.0f, centerPos: GE = 0.0f, dur: GE = 0.1f, pan: GE = 0.0f, amp: GE = 0.1f, interp: GE = 4)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, buf.expand, speed.expand, centerPos.expand, dur.expand, pan.expand, amp.expand, interp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}
object ScopeOut extends Reader[ScopeOut] {
  def kr(buf: GE, in: GE): ScopeOut = new ScopeOut(control, buf, in)
  
  def ar(buf: GE, in: GE): ScopeOut = new ScopeOut(audio, buf, in)
  
  def read(in: DataInput): ScopeOut = {
    readArity(in, 3)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    val _in   = readGE(in)
    new ScopeOut(_rate, _buf, _in)
  }
}
final case class ScopeOut(rate: Rate, buf: GE, in: GE)
  extends UGenSource.ZeroOut with HasSideEffect with IsIndividual {

  protected def makeUGens: Unit = unwrap(this, Vector[UGenInLike](buf.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = UGen.ZeroOut(name, rate, _args, isIndividual = true)
}
object ScopeOut2 extends Reader[ScopeOut2] {
  def kr(buf: GE, in: GE, maxFrames: GE = 4096, frames: GE): ScopeOut2 = 
    new ScopeOut2(control, buf, in, maxFrames, frames)
  
  def ar(buf: GE, in: GE, maxFrames: GE = 4096, frames: GE): ScopeOut2 = 
    new ScopeOut2(audio, buf, in, maxFrames, frames)
  
  def read(in: DataInput): ScopeOut2 = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _in         = readGE(in)
    val _maxFrames  = readGE(in)
    val _frames     = readGE(in)
    new ScopeOut2(_rate, _buf, _in, _maxFrames, _frames)
  }
}
final case class ScopeOut2(rate: Rate, buf: GE, in: GE, maxFrames: GE = 4096, frames: GE)
  extends UGenSource.ZeroOut with HasSideEffect with IsIndividual {

  protected def makeUGens: Unit = 
    unwrap(this, Vector[UGenInLike](buf.expand, maxFrames.expand, frames.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = UGen.ZeroOut(name, rate, _args, isIndividual = true)
}

/** A Karplus-Strong UGen.
  */
object Pluck extends Reader[Pluck] {
  /** @param in               An excitation signal.
    * @param trig             Upon a negative to positive transition, the excitation
    *                         signal will be fed into the delay line.
    * @param maxDelayTime     Maximum delay time in seconds (initializes the internal
    *                         delay buffer). ''(init-time only)''
    * @param delayTime        Delay time in seconds.
    * @param decayTime        Time for the echoes to decay by 60 decibels. Negative
    *                         times emphasize odd partials.
    * @param coeff            the coefficient of the internal OnePole filter. Values
    *                         should be between -1 and +1 (larger values will be
    *                         unstable... so be careful!).
    */
  def ar(in: GE, trig: GE = 1, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f, coeff: GE = 0.5f): Pluck = 
    new Pluck(in, trig, maxDelayTime, delayTime, decayTime, coeff)
  
  def read(in: DataInput): Pluck = {
    readArity(in, 6)
    val _in           = readGE(in)
    val _trig         = readGE(in)
    val _maxDelayTime = readGE(in)
    val _delayTime    = readGE(in)
    val _decayTime    = readGE(in)
    val _coeff        = readGE(in)
    new Pluck(_in, _trig, _maxDelayTime, _delayTime, _decayTime, _coeff)
  }
}

/** A Karplus-Strong UGen.
  * 
  * @param in               An excitation signal.
  * @param trig             Upon a negative to positive transition, the excitation
  *                         signal will be fed into the delay line.
  * @param maxDelayTime     Maximum delay time in seconds (initializes the internal
  *                         delay buffer). ''(init-time only)''
  * @param delayTime        Delay time in seconds.
  * @param decayTime        Time for the echoes to decay by 60 decibels. Negative
  *                         times emphasize odd partials.
  * @param coeff            the coefficient of the internal OnePole filter. Values
  *                         should be between -1 and +1 (larger values will be
  *                         unstable... so be careful!).
  */
final case class Pluck(in: GE, trig: GE = 1, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1.0f, coeff: GE = 0.5f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, trig.expand, maxDelayTime.expand, delayTime.expand, decayTime.expand, coeff.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, audio, _args)
}

/** Write to a buffer for a `DelTapRd` UGen
  * 
  * @see [[de.sciss.synth.ugen.DelTapRd$ DelTapRd]]
  */
object DelTapWr extends Reader[DelTapWr] {
  /** @param buf              The buffer to write signal into. Max delay time is
    *                         based on buffer size. `DelTapWr` outputs its current
    *                         sample value for use in the `phase` argument of
    *                         `DelTapRd` .
    * @param in               The input signal.
    */
  def kr(buf: GE, in: GE): DelTapWr = new DelTapWr(control, buf, in)
  
  /** @param buf              The buffer to write signal into. Max delay time is
    *                         based on buffer size. `DelTapWr` outputs its current
    *                         sample value for use in the `phase` argument of
    *                         `DelTapRd` .
    * @param in               The input signal.
    */
  def ar(buf: GE, in: GE): DelTapWr = new DelTapWr(audio, buf, in)
  
  def read(in: DataInput): DelTapWr = {
    readArity(in, 3)
    val _rate = readRate(in)
    val _buf  = readGE(in)
    val _in   = readGE(in)
    new DelTapWr(_rate, _buf, _in)
  }
}

/** Write to a buffer for a `DelTapRd` UGen
  * 
  * @param buf              The buffer to write signal into. Max delay time is
  *                         based on buffer size. `DelTapWr` outputs its current
  *                         sample value for use in the `phase` argument of
  *                         `DelTapRd` .
  * @param in               The input signal.
  * 
  * @see [[de.sciss.synth.ugen.DelTapRd$ DelTapRd]]
  */
final case class DelTapWr(rate: Rate, buf: GE, in: GE)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, rate, _args, hasSideEffect = true, isIndividual = true)
}

/** Tap a delay line from a `DelTapWr` UGen.
  * 
  * @see [[de.sciss.synth.ugen.DelTapWr$ DelTapWr]]
  */
object DelTapRd extends Reader[DelTapRd] {
  /** @param buf              Buffer where DelTapWr has written signal. Max delay
    *                         time is based on buffer size.
    * @param phase            the current phase of the DelTapWr UGen. This is the
    *                         output of DelTapWr.
    * @param delayTime        Delay time in seconds.
    * @param interp           The kind of interpolation to be used. 1 is none, 2 is
    *                         linear, 4 is cubic..
    */
  def kr(buf: GE, phase: GE, delayTime: GE, interp: GE = 1): DelTapRd = 
    new DelTapRd(control, buf, phase, delayTime, interp)
  
  /** @param buf              Buffer where DelTapWr has written signal. Max delay
    *                         time is based on buffer size.
    * @param phase            the current phase of the DelTapWr UGen. This is the
    *                         output of DelTapWr.
    * @param delayTime        Delay time in seconds.
    * @param interp           The kind of interpolation to be used. 1 is none, 2 is
    *                         linear, 4 is cubic..
    */
  def ar(buf: GE, phase: GE, delayTime: GE, interp: GE = 1): DelTapRd = 
    new DelTapRd(audio, buf, phase, delayTime, interp)
  
  def read(in: DataInput): DelTapRd = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _buf        = readGE(in)
    val _phase      = readGE(in)
    val _delayTime  = readGE(in)
    val _interp     = readGE(in)
    new DelTapRd(_rate, _buf, _phase, _delayTime, _interp)
  }
}

/** Tap a delay line from a `DelTapWr` UGen.
  * 
  * @param buf              Buffer where DelTapWr has written signal. Max delay
  *                         time is based on buffer size.
  * @param phase            the current phase of the DelTapWr UGen. This is the
  *                         output of DelTapWr.
  * @param delayTime        Delay time in seconds.
  * @param interp           The kind of interpolation to be used. 1 is none, 2 is
  *                         linear, 4 is cubic..
  * 
  * @see [[de.sciss.synth.ugen.DelTapWr$ DelTapWr]]
  */
final case class DelTapRd(rate: Rate, buf: GE, phase: GE, delayTime: GE, interp: GE = 1)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, phase.expand, delayTime.expand, interp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A scalar (init-time) UGen that overwrites contents of a buffer with given
  * values.
  * 
  * @see [[de.sciss.synth.ugen.LocalBuf$ LocalBuf]]
  * @see [[de.sciss.synth.ugen.ClearBuf$ ClearBuf]]
  */
object SetBuf extends Reader[SetBuf] {
  /** @param buf              the buffer to write the values into
    * @param values           values to write into the buffer
    * @param offset           frame offset into the buffer
    */
  def ir(buf: GE, values: GE, offset: GE = 0): SetBuf = new SetBuf(buf, values, offset)
  
  def read(in: DataInput): SetBuf = {
    readArity(in, 3)
    val _buf    = readGE(in)
    val _values = readGE(in)
    val _offset = readGE(in)
    new SetBuf(_buf, _values, _offset)
  }
}

/** A scalar (init-time) UGen that overwrites contents of a buffer with given
  * values.
  * 
  * @param buf              the buffer to write the values into
  * @param values           values to write into the buffer
  * @param offset           frame offset into the buffer
  * 
  * @see [[de.sciss.synth.ugen.LocalBuf$ LocalBuf]]
  * @see [[de.sciss.synth.ugen.ClearBuf$ ClearBuf]]
  */
final case class SetBuf(buf: GE, values: GE, offset: GE = 0)
  extends UGenSource.SingleOut with ScalarRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, offset.expand).++({
      val _exp = values.expand.outputs
      _exp.+:(Constant(_exp.size))
    }))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, scalar, _args, hasSideEffect = true, isIndividual = true)
}

/** A scalar (init-time) UGen that fills the contents of a buffer with zeroes.
  * 
  * @see [[de.sciss.synth.ugen.LocalBuf$ LocalBuf]]
  * @see [[de.sciss.synth.ugen.SetBuf$ SetBuf]]
  */
object ClearBuf extends Reader[ClearBuf] {
  /** @param buf              the buffer to clear
    */
  def ir(buf: GE): ClearBuf = new ClearBuf(buf)
  
  def read(in: DataInput): ClearBuf = {
    readArity(in, 1)
    val _buf = readGE(in)
    new ClearBuf(_buf)
  }
}

/** A scalar (init-time) UGen that fills the contents of a buffer with zeroes.
  * 
  * @param buf              the buffer to clear
  * 
  * @see [[de.sciss.synth.ugen.LocalBuf$ LocalBuf]]
  * @see [[de.sciss.synth.ugen.SetBuf$ SetBuf]]
  */
final case class ClearBuf(buf: GE)
  extends UGenSource.SingleOut with ScalarRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, scalar, _args, hasSideEffect = true, isIndividual = true)
}