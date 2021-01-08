// revision: 3
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that writes a signal onto a bus, delaying the signal such that the input
  * will begin to appear on the bus precisely when the encompassing Synth was
  * scheduled according to its OSC bundle. I.e. if the synth is scheduled to be
  * started part way through a control cycle, `OffsetOut` will maintain the correct
  * offset by buffering the output and delaying it until the exact time that the
  * synth was scheduled for.
  * 
  * This UGen adds ("mixes") the input-signal to the existing contents of the bus.
  * 
  * Multi-channel input signals, for example a `PanAz` , are written as such to the
  * bus without expansion. That is, the `bus` index argument is used for the first
  * channel, the second channel will appear on `bus + 1` , etc.
  * 
  * If you have an expanding multi-channel input, however, you have to be careful.
  * For example, if you have
  * `PanAz.ar(2, SinOsc.ar(Seq(444, 555, 666)) * 0.2, Seq(-1, 0, 1))` , this results
  * in one output UGen carrying one channel, and another one carrying two channels.
  * (The way this works is consistent with SCLang). In order to get the correct
  * behaviour (left outputs of the `PanAz` summed, and right output of the `PanAz`
  * summed), wrap this expression in a `Mix(...)` before passing it to the output
  * UGen.
  * 
  * '''Note''': You cannot currently achieve sample accurate scheduling in
  * SuperCollider. This UGen is therefore more or less useless.
  * 
  * ===Examples===
  * 
  * {{{
  * // compare left-right
  * val sd = SynthDef.recv("offset-out") {
  *   val x = Impulse.ar(2)
  *   val y = SubsampleOffset.ir
  *   y.poll(0, "offset")
  *   Out      .ar(0, x)
  *   OffsetOut.ar(1, x)    // right channel will be delayed against left
  * }
  * 
  * val x = Synth(s)
  * s ! osc.Bundle.millis(System.currentTimeMillis + 1000, x.newMsg(sd.name, s))
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.ReplaceOut$ ReplaceOut]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  * @see [[de.sciss.synth.ugen.SubsampleOffset$ SubsampleOffset]]
  */
object OffsetOut extends Reader[OffsetOut] {
  /** @param bus              bus index to write to. For an audio-rate UGen, this is
    *                         an audio-bus, for a control-rate UGen, this is a
    *                         control-bus.
    * @param in               signal to write to the bus. If the UGen is audio-rate,
    *                         the input must also be audio-rate.
    */
  def ar(bus: GE, in: GE): OffsetOut = new OffsetOut(bus, in)
  
  def read(in: DataInput): OffsetOut = {
    readArity(in, 2)
    val _bus  = readGE(in)
    val _in   = readGE(in)
    new OffsetOut(_bus, _in)
  }
}

/** A UGen that writes a signal onto a bus, delaying the signal such that the input
  * will begin to appear on the bus precisely when the encompassing Synth was
  * scheduled according to its OSC bundle. I.e. if the synth is scheduled to be
  * started part way through a control cycle, `OffsetOut` will maintain the correct
  * offset by buffering the output and delaying it until the exact time that the
  * synth was scheduled for.
  * 
  * This UGen adds ("mixes") the input-signal to the existing contents of the bus.
  * 
  * Multi-channel input signals, for example a `PanAz` , are written as such to the
  * bus without expansion. That is, the `bus` index argument is used for the first
  * channel, the second channel will appear on `bus + 1` , etc.
  * 
  * If you have an expanding multi-channel input, however, you have to be careful.
  * For example, if you have
  * `PanAz.ar(2, SinOsc.ar(Seq(444, 555, 666)) * 0.2, Seq(-1, 0, 1))` , this results
  * in one output UGen carrying one channel, and another one carrying two channels.
  * (The way this works is consistent with SCLang). In order to get the correct
  * behaviour (left outputs of the `PanAz` summed, and right output of the `PanAz`
  * summed), wrap this expression in a `Mix(...)` before passing it to the output
  * UGen.
  * 
  * '''Note''': You cannot currently achieve sample accurate scheduling in
  * SuperCollider. This UGen is therefore more or less useless.
  * 
  * @param bus              bus index to write to. For an audio-rate UGen, this is
  *                         an audio-bus, for a control-rate UGen, this is a
  *                         control-bus.
  * @param in               signal to write to the bus. If the UGen is audio-rate,
  *                         the input must also be audio-rate.
  * 
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.ReplaceOut$ ReplaceOut]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  * @see [[de.sciss.synth.ugen.SubsampleOffset$ SubsampleOffset]]
  */
final case class OffsetOut(bus: GE, in: GE)
  extends UGenSource.ZeroOut with AudioRated with HasSideEffect with IsIndividual {

  protected def makeUGens: Unit = unwrap(this, Vector[UGenInLike](bus.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _args1 = if (audio.==(audio)) matchRateFrom(_args, 1, audio) else _args
    UGen.ZeroOut(name, audio, _args1, isIndividual = true)
  }
}

/** A UGen that reads buses that are local to the enclosing synth. These buses
  * should be written using a `LocalOut` ugen. They behave like regular buses, but
  * are more convenient for the implementation of a self contained effect that uses
  * a feedback processing loop.
  * 
  * In a synth, there can be only each one control-rate and audio-rate `LocalIn` /
  * `LocalOut` pair. The signal written to a `LocalOut` will not be read by the
  * `LocalIn` until the next control block cycle, introducing a delay of
  * `ControlDur` .
  * 
  * '''Warning''': The argument has been changed `numChannels: Int` in version
  * 1.15.3 to `init: GE` in version 1.16.0. The previous version was incompatible
  * with SuperCollider 3.6.x. A previous usage such as `LocalIn.ar(2)` to create two
  * channels must now be expressed as `LocalIn.ar(Seq(0, 0))` !
  * 
  * ===Examples===
  * 
  * {{{
  * // ping-pong delay with feedback
  * play {
  *   val src = Decay.ar(Impulse.ar(0.3), 0.1) * WhiteNoise.ar(0.2)
  *   val in  = LocalIn.ar(Seq(0, 0)) + Seq[GE](src, 0) // read feedback, add source to left chan
  *   val dly = DelayN.ar(in, 0.2, 0.2)         // delay sound
  *   val att = dly * 0.8                       // apply decay factor
  *   
  *   // reverse channels to give ping pong effect
  *   LocalOut.ar(Seq(att.out(1), dly.out(0)))
  *   dly
  * }
  * }}}
  * {{{
  * // tank
  * play {
  *   val tones = Mix.fill(12) {
  *     Pan2.ar(
  *       Decay2.ar(Dust.ar(0.05), 0.1, 0.5) * 0.1 *
  *         FSinOsc.ar(IRand(36,84).midiCps).cubed.max(0),
  *       Rand(-1,1))
  *   }
  *   val gen = tones + Pan2.ar(Decay2.ar(Dust.ar(0.03), 0.04, 0.3) * BrownNoise.ar, 0)
  *   
  *   val verb = Mix.fold(gen, 4) { z =>
  *     AllpassN.ar(z, 0.03, Seq.fill(2)(Rand(0.005,0.02)), 1)
  *   }
  *   
  *   val in  = LocalIn.ar(Seq(0, 0)) * 0.98
  *   val flt = OnePole.ar(in, 0.5)
  *   
  *   val pan = Rotate2.ar(flt.out(0), flt.out(1), 0.23)
  *   val v1  = AllpassN.ar(pan, 0.05, Seq.fill(2)(Rand(0.01,0.05)), 2)
  *   val v2  = DelayN.ar(v1, 0.3, Seq(0.19,0.26))
  *   val v3  = AllpassN.ar(v2 , 0.05, Seq.fill(2)(Rand(0.03,0.15)), 2)
  *   val out = LeakDC.ar(v3)
  *   val sig = gen + out
  *   
  *   LocalOut.ar(sig)
  *   sig
  * }
  * }}}
  * {{{
  * // resonator
  * play {
  *   val imp  = Impulse.ar
  *   val in   = LocalIn.ar
  *   val feed = imp + in * 0.995
  *   // must subtract block-size for correct tuning
  *   // (try removing the ControlDur to here the pitch change)
  *   val time = 440.reciprocal - ControlDur.ir
  *   val dly  = DelayC.ar(feed, time, time)
  *   LocalOut.ar(dly)
  *   
  *   // alternate between feedback and reference pitch
  *   val comp = Seq(dly, SinOsc.ar(440) * 0.2): GE
  *   comp * LFPulse.kr(1, Seq(0.0, 0.5))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LocalOut$ LocalOut]]
  * @see [[de.sciss.synth.ugen.InFeedback$ InFeedback]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
object LocalIn extends Reader[LocalIn] {
  def kr: LocalIn = kr()
  
  /** @param init             the initial state of the UGen. The number of channels
    *                         of this signal should match with the number of channels
    *                         written via `LocalOut` .
    */
  def kr(init: GE = 0): LocalIn = new LocalIn(control, init)
  
  def ar: LocalIn = ar()
  
  /** @param init             the initial state of the UGen. The number of channels
    *                         of this signal should match with the number of channels
    *                         written via `LocalOut` .
    */
  def ar(init: GE = 0): LocalIn = new LocalIn(audio, init)
  
  def read(in: DataInput): LocalIn = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _init = readGE(in)
    new LocalIn(_rate, _init)
  }
}

/** A UGen that reads buses that are local to the enclosing synth. These buses
  * should be written using a `LocalOut` ugen. They behave like regular buses, but
  * are more convenient for the implementation of a self contained effect that uses
  * a feedback processing loop.
  * 
  * In a synth, there can be only each one control-rate and audio-rate `LocalIn` /
  * `LocalOut` pair. The signal written to a `LocalOut` will not be read by the
  * `LocalIn` until the next control block cycle, introducing a delay of
  * `ControlDur` .
  * 
  * '''Warning''': The argument has been changed `numChannels: Int` in version
  * 1.15.3 to `init: GE` in version 1.16.0. The previous version was incompatible
  * with SuperCollider 3.6.x. A previous usage such as `LocalIn.ar(2)` to create two
  * channels must now be expressed as `LocalIn.ar(Seq(0, 0))` !
  * 
  * @param init             the initial state of the UGen. The number of channels
  *                         of this signal should match with the number of channels
  *                         written via `LocalOut` .
  * 
  * @see [[de.sciss.synth.ugen.LocalOut$ LocalOut]]
  * @see [[de.sciss.synth.ugen.InFeedback$ InFeedback]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
final case class LocalIn(rate: Rate, init: GE = 0) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, init.expand.outputs)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(_args.size)(rate), _args)
}

/** A UGen that cross-fades the contents of a bus with an input signal. A linear
  * cross-fade can go from 0.0 (previous bus contents preserved, no input signal
  * added) via 0.5 (previous signal attenuated by -6 dB, input signal attenuated by
  * -6 dB and added) to 1.0 (contents completely replaced by input signal).
  * 
  * ===Examples===
  * 
  * {{{
  * // cross-fade two synths
  * val sin = play {
  *   Out.ar(0, SinOsc.ar(440) * 0.1)
  * }
  * 
  * val noise = play(target = sin, addAction = addAfter) {
  *   XOut.ar(0, PinkNoise.ar(0.1), xfade = "xfade".kr(0))
  * }
  * 
  * noise.set("xfade" -> 0.5) // both signals heard
  * noise.set("xfade" -> 1.0) // just noise
  * noise.set("xfade" -> 0.0) // just sine
  * }}}
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
object XOut extends Reader[XOut] {
  /** @param bus              bus index to write to. For an audio-rate UGen, this is
    *                         an audio-bus, for a control-rate UGen, this is a
    *                         control-bus.
    * @param in               signal to write to the bus. If the UGen is audio-rate,
    *                         the input must also be audio-rate.
    * @param xfade            cross-fade value. The new bus contents will be
    *                         `old_bus_content * (1 - xfade) + in * xfade`
    */
  def kr(bus: GE, in: GE, xfade: GE): XOut = new XOut(control, bus, in, xfade)
  
  /** @param bus              bus index to write to. For an audio-rate UGen, this is
    *                         an audio-bus, for a control-rate UGen, this is a
    *                         control-bus.
    * @param in               signal to write to the bus. If the UGen is audio-rate,
    *                         the input must also be audio-rate.
    * @param xfade            cross-fade value. The new bus contents will be
    *                         `old_bus_content * (1 - xfade) + in * xfade`
    */
  def ar(bus: GE, in: GE, xfade: GE): XOut = new XOut(audio, bus, in, xfade)
  
  def read(in: DataInput): XOut = {
    readArity(in, 4)
    val _rate   = readRate(in)
    val _bus    = readGE(in)
    val _in     = readGE(in)
    val _xfade  = readGE(in)
    new XOut(_rate, _bus, _in, _xfade)
  }
}

/** A UGen that cross-fades the contents of a bus with an input signal. A linear
  * cross-fade can go from 0.0 (previous bus contents preserved, no input signal
  * added) via 0.5 (previous signal attenuated by -6 dB, input signal attenuated by
  * -6 dB and added) to 1.0 (contents completely replaced by input signal).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param bus              bus index to write to. For an audio-rate UGen, this is
  *                         an audio-bus, for a control-rate UGen, this is a
  *                         control-bus.
  * @param in               signal to write to the bus. If the UGen is audio-rate,
  *                         the input must also be audio-rate.
  * @param xfade            cross-fade value. The new bus contents will be
  *                         `old_bus_content * (1 - xfade) + in * xfade`
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
final case class XOut(rate: Rate, bus: GE, in: GE, xfade: GE)
  extends UGenSource.ZeroOut with HasSideEffect with IsIndividual {

  protected def makeUGens: Unit = unwrap(this, Vector[UGenInLike](bus.expand, xfade.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _args1 = if (rate.==(audio)) matchRateFrom(_args, 2, audio) else _args
    UGen.ZeroOut(name, rate, _args1, isIndividual = true)
  }
}

/** A UGen that replace the contents of a bus with an input signal. Other than
  * `Out` , the signal is not added to the previous contents of the bus but replaces
  * it, allowing for a simple way of an "insert" effect.
  * 
  * Multi-channel input signals, for example a `PanAz` , are written as such to the
  * bus without expansion. That is, the `bus` index argument is used for the first
  * channel, the second channel will appear on `bus + 1` , etc.
  * 
  * If you have an expanding multi-channel input, however, you have to be careful.
  * For example, if you have
  * `PanAz.ar(2, SinOsc.ar(Seq(444, 555, 666)) * 0.2, Seq(-1, 0, 1))` , this results
  * in one output UGen carrying one channel, and another one carrying two channels.
  * (The way this works is consistent with SCLang). In order to get the correct
  * behaviour (left outputs of the `PanAz` summed, and right output of the `PanAz`
  * summed), wrap this expression in a `Mix(...)` before passing it to the output
  * UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // insert-effect
  * val noise = play {
  *   Out.ar(0, WhiteNoise.ar(Seq(0.1, 0.1)))
  * }
  * 
  * val filter = play(target = noise, addAction = addAfter) {
  *   val in = In.ar(0, 2)
  *   val f  = Resonz.ar(in, 444, 0.1) * 20
  *   ReplaceOut.ar(0, f)
  * }
  * 
  * filter.run(false)  // bypass
  * filter.run(true )  // engage
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
object ReplaceOut extends Reader[ReplaceOut] {
  /** @param bus              bus index to write to. For an audio-rate UGen, this is
    *                         an audio-bus, for a control-rate UGen, this is a
    *                         control-bus.
    * @param in               signal to write to the bus. If the UGen is audio-rate,
    *                         the input must also be audio-rate.
    */
  def ar(bus: GE, in: GE): ReplaceOut = new ReplaceOut(bus, in)
  
  def read(in: DataInput): ReplaceOut = {
    readArity(in, 2)
    val _bus  = readGE(in)
    val _in   = readGE(in)
    new ReplaceOut(_bus, _in)
  }
}

/** A UGen that replace the contents of a bus with an input signal. Other than
  * `Out` , the signal is not added to the previous contents of the bus but replaces
  * it, allowing for a simple way of an "insert" effect.
  * 
  * Multi-channel input signals, for example a `PanAz` , are written as such to the
  * bus without expansion. That is, the `bus` index argument is used for the first
  * channel, the second channel will appear on `bus + 1` , etc.
  * 
  * If you have an expanding multi-channel input, however, you have to be careful.
  * For example, if you have
  * `PanAz.ar(2, SinOsc.ar(Seq(444, 555, 666)) * 0.2, Seq(-1, 0, 1))` , this results
  * in one output UGen carrying one channel, and another one carrying two channels.
  * (The way this works is consistent with SCLang). In order to get the correct
  * behaviour (left outputs of the `PanAz` summed, and right output of the `PanAz`
  * summed), wrap this expression in a `Mix(...)` before passing it to the output
  * UGen.
  * 
  * @param bus              bus index to write to. For an audio-rate UGen, this is
  *                         an audio-bus, for a control-rate UGen, this is a
  *                         control-bus.
  * @param in               signal to write to the bus. If the UGen is audio-rate,
  *                         the input must also be audio-rate.
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
final case class ReplaceOut(bus: GE, in: GE)
  extends UGenSource.ZeroOut with AudioRated with HasSideEffect with IsIndividual {

  protected def makeUGens: Unit = unwrap(this, Vector[UGenInLike](bus.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _args1 = if (audio.==(audio)) matchRateFrom(_args, 1, audio) else _args
    UGen.ZeroOut(name, audio, _args1, isIndividual = true)
  }
}

/** A UGen that writes a signal onto a bus. It adds ("mixes") the input-signal to
  * the existing contents of the bus.
  * 
  * Multi-channel input signals, for example a `PanAz` , are written as such to the
  * bus without expansion. That is, the `bus` index argument is used for the first
  * channel, the second channel will appear on `bus + 1` , etc.
  * 
  * If you have an expanding multi-channel input, however, you have to be careful.
  * For example, if you have
  * `PanAz.ar(2, SinOsc.ar(Seq(444, 555, 666)) * 0.2, Seq(-1, 0, 1))` , this results
  * in one output UGen carrying one channel, and another one carrying two channels.
  * (The way this works is consistent with SCLang). In order to get the correct
  * behaviour (left outputs of the `PanAz` summed, and right output of the `PanAz`
  * summed), wrap this expression in a `Mix(...)` before passing it to the output
  * UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // cross-synth routing
  * // allocate an internal stereo audio-bus
  * val bus = Bus.audio(s, 2)
  * 
  * // writes to internal bus (initially inaudible)
  * val x = play {
  *   Out.ar(bus.index, Dust.ar(Seq(345, 345)))
  * }
  * 
  * // reads internal bus and makes it audible.
  * // must be after `x` to be able to read the bus signal
  * val y = play(target = x, addAction = addAfter) {
  *   val in = In.ar(bus.index, 2)
  *   Resonz.ar(in, 555, 0.1) * 10
  * }
  * 
  * // when done, do not forget to free the bus
  * y.free(); x.free(); bus.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.PhysicalOut$ PhysicalOut]]
  * @see [[de.sciss.synth.ugen.ReplaceOut$ ReplaceOut]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  * @see [[de.sciss.synth.ugen.LocalOut$ LocalOut]]
  * @see [[de.sciss.synth.Bus Bus]]
  */
object Out extends Reader[Out] {
  /** @param bus              bus index to write to. For an audio-rate UGen, this is
    *                         an audio-bus, for a control-rate UGen, this is a
    *                         control-bus. '''Note''' that the bus index can only be
    *                         modulated at control-rate.
    * @param in               signal to write to the bus. If the UGen is audio-rate,
    *                         the input must also be audio-rate.
    */
  def kr(bus: GE, in: GE): Out = new Out(control, bus, in)
  
  /** @param bus              bus index to write to. For an audio-rate UGen, this is
    *                         an audio-bus, for a control-rate UGen, this is a
    *                         control-bus. '''Note''' that the bus index can only be
    *                         modulated at control-rate.
    * @param in               signal to write to the bus. If the UGen is audio-rate,
    *                         the input must also be audio-rate.
    */
  def ar(bus: GE, in: GE): Out = new Out(audio, bus, in)
  
  def read(in: DataInput): Out = {
    readArity(in, 3)
    val _rate = readRate(in)
    val _bus  = readGE(in)
    val _in   = readGE(in)
    new Out(_rate, _bus, _in)
  }
}

/** A UGen that writes a signal onto a bus. It adds ("mixes") the input-signal to
  * the existing contents of the bus.
  * 
  * Multi-channel input signals, for example a `PanAz` , are written as such to the
  * bus without expansion. That is, the `bus` index argument is used for the first
  * channel, the second channel will appear on `bus + 1` , etc.
  * 
  * If you have an expanding multi-channel input, however, you have to be careful.
  * For example, if you have
  * `PanAz.ar(2, SinOsc.ar(Seq(444, 555, 666)) * 0.2, Seq(-1, 0, 1))` , this results
  * in one output UGen carrying one channel, and another one carrying two channels.
  * (The way this works is consistent with SCLang). In order to get the correct
  * behaviour (left outputs of the `PanAz` summed, and right output of the `PanAz`
  * summed), wrap this expression in a `Mix(...)` before passing it to the output
  * UGen.
  * 
  * @param bus              bus index to write to. For an audio-rate UGen, this is
  *                         an audio-bus, for a control-rate UGen, this is a
  *                         control-bus. '''Note''' that the bus index can only be
  *                         modulated at control-rate.
  * @param in               signal to write to the bus. If the UGen is audio-rate,
  *                         the input must also be audio-rate.
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.PhysicalOut$ PhysicalOut]]
  * @see [[de.sciss.synth.ugen.ReplaceOut$ ReplaceOut]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  * @see [[de.sciss.synth.ugen.LocalOut$ LocalOut]]
  * @see [[de.sciss.synth.Bus Bus]]
  */
final case class Out(rate: Rate, bus: GE, in: GE)
  extends UGenSource.ZeroOut with HasSideEffect with IsIndividual {

  protected def makeUGens: Unit = unwrap(this, Vector[UGenInLike](bus.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _args1 = if (rate.==(audio)) matchRateFrom(_args, 1, audio) else _args
    UGen.ZeroOut(name, rate, _args1, isIndividual = true)
  }
}

/** A UGen that writes to buses that are local to the enclosing synth. These buses
  * should have been defined by a `LocalIn` ugen. These behave like regular buses,
  * but are more convenient for the implementation of a self contained effect that
  * uses a feedback processing loop.
  * 
  * In a synth, there can be only each one control-rate and audio-rate `LocalIn` /
  * `LocalOut` pair. The signal written to a `LocalOut` will not be read by the
  * `LocalIn` until the next control block cycle, introducing a delay of
  * `ControlDur` .
  * 
  * For more examples, see `LocalIn`.
  * 
  * ===Examples===
  * 
  * {{{
  * // ping-pong delay with feedback
  * play {
  *   val src = Decay.ar(Impulse.ar(0.3), 0.1) * WhiteNoise.ar(0.2)
  *   val in  = LocalIn.ar(Seq(0, 0)) + Seq[GE](src, 0) // read feedback, add source to left chan
  *   val dly = DelayN.ar(in, 0.2, 0.2)         // delay sound
  *   val att = dly * 0.8                       // apply decay factor
  *   
  *   // reverse channels to give ping pong effect
  *   LocalOut.ar(Seq(att.out(1), dly.out(0)))
  *   dly
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LocalIn$ LocalIn]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
object LocalOut extends Reader[LocalOut] {
  /** @param in               signal to be written to the synth-local bus. The
    *                         signal's number of channels must be the same number of
    *                         channels as were declared in the corresponding `LocalIn`
    *                         .
    */
  def kr(in: GE): LocalOut = new LocalOut(control, in)
  
  /** @param in               signal to be written to the synth-local bus. The
    *                         signal's number of channels must be the same number of
    *                         channels as were declared in the corresponding `LocalIn`
    *                         .
    */
  def ar(in: GE): LocalOut = new LocalOut(audio, in)
  
  def read(in: DataInput): LocalOut = {
    readArity(in, 2)
    val _rate = readRate(in)
    val _in   = readGE(in)
    new LocalOut(_rate, _in)
  }
}

/** A UGen that writes to buses that are local to the enclosing synth. These buses
  * should have been defined by a `LocalIn` ugen. These behave like regular buses,
  * but are more convenient for the implementation of a self contained effect that
  * uses a feedback processing loop.
  * 
  * In a synth, there can be only each one control-rate and audio-rate `LocalIn` /
  * `LocalOut` pair. The signal written to a `LocalOut` will not be read by the
  * `LocalIn` until the next control block cycle, introducing a delay of
  * `ControlDur` .
  * 
  * For more examples, see `LocalIn`.
  * 
  * @param in               signal to be written to the synth-local bus. The
  *                         signal's number of channels must be the same number of
  *                         channels as were declared in the corresponding `LocalIn`
  *                         .
  * 
  * @see [[de.sciss.synth.ugen.LocalIn$ LocalIn]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
final case class LocalOut(rate: Rate, in: GE) extends UGenSource.ZeroOut {
  protected def makeUGens: Unit = unwrap(this, in.expand.outputs)
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _args1 = if (rate.==(audio)) matchRateFrom(_args, 0, audio) else _args
    UGen.ZeroOut(name, rate, _args1)
  }
}

/** A UGen that reads a signal from a bus. Whether an audio- or control-bus is used
  * depends on the rate of the UGen.
  * 
  * `In.ar` and `In.kr` behave differently with respect to signals left on the bus
  * in the previous calculation cycle (control block): `In.ar` can access audio
  * signals that were generated in the current calculation cycle by synths appearing
  * earlier in the node tree. It does not read signals produced by nodes in the
  * previous calculation cycle (i.e. synths appearing later in the node tree), the
  * input would instead be zero. To allow such "feedback", `InFeedback` can be used.
  * 
  * In contrast, `In.kr` does not distinguish between "new" and "old" data: It will
  * always read the most recent value found on the bus, whether it was generated
  * earlier in this calculation cycle, left over from the last one, or set before by
  * the client.
  * 
  * '''Note''': The server uses the first `NumOutputBuses` channels to write to the
  * sound card, followed by another `NumInputBuses` to read from the sound card. For
  * convenience, the pseudo-UGens `PhysicalOut` and `PhysicalIn` can be used.
  * 
  * ===Examples===
  * 
  * {{{
  * // cross-synth routing
  * // allocate an internal stereo audio-bus
  * val bus = Bus.audio(s, 2)
  * 
  * // writes to internal bus (initially inaudible)
  * val x = play {
  *   Out.ar(bus.index, Dust.ar(Seq(345, 345)))
  * }
  * 
  * // reads internal bus and makes it audible.
  * // must be after `x` to be able to read the bus signal
  * val y = play(target = x, addAction = addAfter) {
  *   val in = In.ar(bus.index, 2)
  *   Resonz.ar(in, 555, 0.1) * 10
  * }
  * 
  * // when done, do not forget to free the bus
  * y.free(); x.free(); bus.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.PhysicalIn$ PhysicalIn]]
  * @see [[de.sciss.synth.ugen.InFeedback$ InFeedback]]
  * @see [[de.sciss.synth.ugen.LocalIn$ LocalIn]]
  * @see [[de.sciss.synth.Bus Bus]]
  */
object In extends Reader[In] {
  /** @param bus              index of the bus to read from. When `numChannels` is
    *                         greater than one, the other channels or read from the
    *                         adjacent indices.
    * @param numChannels      number of channels to read
    */
  def ir(bus: GE, numChannels: Int = 1): In = new In(scalar, bus, numChannels)
  
  /** @param bus              index of the bus to read from. When `numChannels` is
    *                         greater than one, the other channels or read from the
    *                         adjacent indices.
    * @param numChannels      number of channels to read
    */
  def kr(bus: GE, numChannels: Int = 1): In = new In(control, bus, numChannels)
  
  /** @param bus              index of the bus to read from. When `numChannels` is
    *                         greater than one, the other channels or read from the
    *                         adjacent indices.
    * @param numChannels      number of channels to read
    */
  def ar(bus: GE, numChannels: Int = 1): In = new In(audio, bus, numChannels)
  
  def read(in: DataInput): In = {
    readArity(in, 3)
    val _rate         = readRate(in)
    val _bus          = readGE(in)
    val _numChannels  = readInt(in)
    new In(_rate, _bus, _numChannels)
  }
}

/** A UGen that reads a signal from a bus. Whether an audio- or control-bus is used
  * depends on the rate of the UGen.
  * 
  * `In.ar` and `In.kr` behave differently with respect to signals left on the bus
  * in the previous calculation cycle (control block): `In.ar` can access audio
  * signals that were generated in the current calculation cycle by synths appearing
  * earlier in the node tree. It does not read signals produced by nodes in the
  * previous calculation cycle (i.e. synths appearing later in the node tree), the
  * input would instead be zero. To allow such "feedback", `InFeedback` can be used.
  * 
  * In contrast, `In.kr` does not distinguish between "new" and "old" data: It will
  * always read the most recent value found on the bus, whether it was generated
  * earlier in this calculation cycle, left over from the last one, or set before by
  * the client.
  * 
  * '''Note''': The server uses the first `NumOutputBuses` channels to write to the
  * sound card, followed by another `NumInputBuses` to read from the sound card. For
  * convenience, the pseudo-UGens `PhysicalOut` and `PhysicalIn` can be used.
  * 
  * @param bus              index of the bus to read from. When `numChannels` is
  *                         greater than one, the other channels or read from the
  *                         adjacent indices.
  * @param numChannels      number of channels to read
  * 
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.PhysicalIn$ PhysicalIn]]
  * @see [[de.sciss.synth.ugen.InFeedback$ InFeedback]]
  * @see [[de.sciss.synth.ugen.LocalIn$ LocalIn]]
  * @see [[de.sciss.synth.Bus Bus]]
  */
final case class In(rate: Rate, bus: GE, numChannels: Int = 1)
  extends UGenSource.MultiOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](bus.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(numChannels)(rate), _args, isIndividual = true)
}

/** A UGen that reads a signal from a control bus and applies a lag filter to it.
  * This is essentially the same as `Lag.kr(In.kr(...), time)` .
  * 
  * ===Examples===
  * 
  * {{{
  * // portamento
  * play {
  *   val c = Bus.control(s)
  *   c.set(30)   // initial midi-pitch
  *   
  *   play {
  *     val freq = LagIn.kr(c.index, time = 1).midiCps
  *     SinOsc.ar(freq) * AmpComp.kr(freq) * 0.1
  *   }
  *   
  *   c.set(70)
  *   c.set(100)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
object LagIn extends Reader[LagIn] {
  /** @param bus              index of the bus to read from. When `numChannels` is
    *                         greater than one, the other channels or read from the
    *                         adjacent indices.
    * @param numChannels      number of channels to read
    * @param time             60 dB lag time in seconds.
    */
  def kr(bus: GE, numChannels: Int = 1, time: GE = 0.1f): LagIn = 
    new LagIn(control, bus, numChannels, time)
  
  def read(in: DataInput): LagIn = {
    readArity(in, 4)
    val _rate         = readRate(in)
    val _bus          = readGE(in)
    val _numChannels  = readInt(in)
    val _time         = readGE(in)
    new LagIn(_rate, _bus, _numChannels, _time)
  }
}

/** A UGen that reads a signal from a control bus and applies a lag filter to it.
  * This is essentially the same as `Lag.kr(In.kr(...), time)` .
  * 
  * @param bus              index of the bus to read from. When `numChannels` is
  *                         greater than one, the other channels or read from the
  *                         adjacent indices.
  * @param numChannels      number of channels to read
  * @param time             60 dB lag time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.Out$ Out]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
final case class LagIn(rate: Rate, bus: GE, numChannels: Int = 1, time: GE = 0.1f)
  extends UGenSource.MultiOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](bus.expand, time.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(numChannels)(rate), _args, isIndividual = true)
}

/** A UGen which reads a signal from an audio bus with a current or one cycle old
  * timestamp.
  * 
  * Audio buses adhere to the concept of a cycle timestamp, which increases for
  * each audio block calculated. When the various output ugens ( `Out` , `OffsetOut`
  * , `XOut` ) write data to a bus, they mix it with any data from the current
  * cycle, but overwrite any data from the previous cycle. ( `ReplaceOut` overwrites
  * all data regardless.) Thus depending on node order and what synths are writing
  * to the bus, the data on a given bus may be from the current cycle or be one
  * cycle old at the time of reading.
  * 
  * `In.ar` checks the timestamp of any data it reads in and zeros any data from
  * the previous cycle (for use within that node; the data remains on the bus). This
  * is fine for audio data, as it avoids feedback, but for control data it is useful
  * to be able to read data from any place in the node order. For this reason
  * `In.kr` also reads data that is older than the current cycle.
  * 
  * In some cases one might also want to read audio from a node later in the
  * current node order. This can be achieved with `InFeedback` . It reads from the
  * previous cycle, and hence introduces a '''delay''' of one block size, which by
  * default is 64 sample frames (equal to about 1.45 ms at 44.1 kHz sample rate).
  * 
  * '''Note''' that no delay occurs when the bus contains a signal which has been
  * written already in the current cycle. The delay is only introduced when no
  * present signal exists.
  * 
  * ===Examples===
  * 
  * {{{
  * // feedback frequency modulation
  * play {
  *   val in = InFeedback.ar(0) // read output
  *   SinOsc.ar(in * 1300 + 300) * 0.4
  * }
  * }}}
  * {{{
  * // resonator
  * val bus = Bus.audio(s) // internal feedback bus
  * 
  * val x = play {
  *   val imp  = Impulse.ar(1)
  *   val in   = InFeedback.ar(bus.index)
  *   val feed = imp + in * 0.995
  *   // must subtract block-size for correct tuning
  *   // (try removing the ControlDur to here the pitch change)
  *   val time = 440.reciprocal - ControlDur.ir
  *   val dly  = DelayC.ar(feed, time, time)
  *   Out.ar(bus.index, dly)
  * 
  *   // alternate between feedback and reference pitch
  *   val comp = Seq(dly, SinOsc.ar(440) * 0.2): GE
  *   comp * LFPulse.kr(1, Seq(0.0, 0.5))
  * }
  * 
  * x.free(); bus.free()  // do not forget to free the bus eventually
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.LocalIn$ LocalIn]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
object InFeedback extends Reader[InFeedback] {
  /** @param bus              the index of the audio bus to read in from.
    * @param numChannels      the number of channels (i.e. adjacent buses) to read
    *                         in. Since this is a constant, a change in number of
    *                         channels of the underlying bus must be reflected by
    *                         creating different SynthDefs.
    */
  def ar(bus: GE, numChannels: Int = 1): InFeedback = new InFeedback(bus, numChannels)
  
  def read(in: DataInput): InFeedback = {
    readArity(in, 2)
    val _bus          = readGE(in)
    val _numChannels  = readInt(in)
    new InFeedback(_bus, _numChannels)
  }
}

/** A UGen which reads a signal from an audio bus with a current or one cycle old
  * timestamp.
  * 
  * Audio buses adhere to the concept of a cycle timestamp, which increases for
  * each audio block calculated. When the various output ugens ( `Out` , `OffsetOut`
  * , `XOut` ) write data to a bus, they mix it with any data from the current
  * cycle, but overwrite any data from the previous cycle. ( `ReplaceOut` overwrites
  * all data regardless.) Thus depending on node order and what synths are writing
  * to the bus, the data on a given bus may be from the current cycle or be one
  * cycle old at the time of reading.
  * 
  * `In.ar` checks the timestamp of any data it reads in and zeros any data from
  * the previous cycle (for use within that node; the data remains on the bus). This
  * is fine for audio data, as it avoids feedback, but for control data it is useful
  * to be able to read data from any place in the node order. For this reason
  * `In.kr` also reads data that is older than the current cycle.
  * 
  * In some cases one might also want to read audio from a node later in the
  * current node order. This can be achieved with `InFeedback` . It reads from the
  * previous cycle, and hence introduces a '''delay''' of one block size, which by
  * default is 64 sample frames (equal to about 1.45 ms at 44.1 kHz sample rate).
  * 
  * '''Note''' that no delay occurs when the bus contains a signal which has been
  * written already in the current cycle. The delay is only introduced when no
  * present signal exists.
  * 
  * @param bus              the index of the audio bus to read in from.
  * @param numChannels      the number of channels (i.e. adjacent buses) to read
  *                         in. Since this is a constant, a change in number of
  *                         channels of the underlying bus must be reflected by
  *                         creating different SynthDefs.
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.LocalIn$ LocalIn]]
  * @see [[de.sciss.synth.ugen.ControlDur$ ControlDur]]
  */
final case class InFeedback(bus: GE, numChannels: Int = 1)
  extends UGenSource.MultiOut with AudioRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](bus.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, audio, Vector.fill(numChannels)(audio), _args, isIndividual = true)
}

/** A UGen which generates a trigger anytime a control bus is set.
  * 
  * Any time the bus is "touched" i.e. has its value set (using `"/c_set"` etc.), a
  * single impulse trigger will be generated. Its amplitude is the value that the
  * bus was set to. Note that if a signal is continuously written to that bus, for
  * instance using `Out.kr` , only one initial trigger is generated once that ugen
  * starts writing, but no successive triggers are generated.
  * 
  * ===Examples===
  * 
  * {{{
  * // envelope trigger
  * val c = Bus.control(s)
  * 
  * val x = play {
  *   val tr = InTrig.kr(c.index)
  *   SinOsc.ar * EnvGen.kr(Env.perc, gate = tr, levelScale = tr)
  * }
  * 
  * c.set(1.0)
  * c.set(0.2)
  * c.set(0.1)
  * 
  * x.free(); c.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.TrigControl$ TrigControl]]
  */
object InTrig extends Reader[InTrig] {
  /** @param bus              the index of the control bus to read in from.
    * @param numChannels      the number of channels (i.e. adjacent buses) to read
    *                         in. Since this is a constant, a change in number of
    *                         channels of the underlying bus must be reflected by
    *                         creating different SynthDefs.
    */
  def kr(bus: GE, numChannels: Int = 1): InTrig = new InTrig(bus, numChannels)
  
  def read(in: DataInput): InTrig = {
    readArity(in, 2)
    val _bus          = readGE(in)
    val _numChannels  = readInt(in)
    new InTrig(_bus, _numChannels)
  }
}

/** A UGen which generates a trigger anytime a control bus is set.
  * 
  * Any time the bus is "touched" i.e. has its value set (using `"/c_set"` etc.), a
  * single impulse trigger will be generated. Its amplitude is the value that the
  * bus was set to. Note that if a signal is continuously written to that bus, for
  * instance using `Out.kr` , only one initial trigger is generated once that ugen
  * starts writing, but no successive triggers are generated.
  * 
  * @param bus              the index of the control bus to read in from.
  * @param numChannels      the number of channels (i.e. adjacent buses) to read
  *                         in. Since this is a constant, a change in number of
  *                         channels of the underlying bus must be reflected by
  *                         creating different SynthDefs.
  * 
  * @see [[de.sciss.synth.ugen.In$ In]]
  * @see [[de.sciss.synth.ugen.TrigControl$ TrigControl]]
  */
final case class InTrig(bus: GE, numChannels: Int = 1)
  extends UGenSource.MultiOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](bus.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, control, Vector.fill(numChannels)(control), _args, isIndividual = true)
}