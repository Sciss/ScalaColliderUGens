// revision: 12
package de.sciss.synth
package ugen

import UGenSource._

/** A low frequency oscillator UGen for modelling vibrato. It produces a modulating
  * frequency value in Hertz that can be used as the frequency parameter of another
  * UGen.
  * 
  * By setting more extreme settings, one can get back to the timbres of FM
  * synthesis. One can also add in some noise to the vibrato rate and vibrato size
  * (modulation depth) to make for a more realistic motor pattern.
  * 
  * The vibrato output is a waveform based on a squared envelope shape with four
  * stages marking out 0.0 to 1.0, 1.0 to 0.0, 0.0 to -1.0, and -1.0 back to 0.0.
  * The vibrato rate determines how quickly one moves through these stages.
  * 
  * ===Examples===
  * 
  * {{{
  * // constant setting
  * play { SinOsc.ar(Vibrato.ar(DC.ar(400.0), 1, 0.02)) * 0.2 }
  * }}}
  * {{{
  * // beat and beatVar mouse control
  * play {
  *   val beat    = MouseX.kr(2.0, 100.0)
  *   val beatVar = MouseY.kr(0.0, 1.0)
  *   val freq    = Vibrato.ar(DC.ar(400.0), beat, 0.1, 1.0, 1.0, beatVar, 0.1)
  *   SinOsc.ar(freq) * 0.2
  * }
  * }}}
  */
object Vibrato extends ProductReader[Vibrato] {
  def kr: Vibrato = kr()
  
  /** @param freq             fundamental frequency in Hertz. If the Vibrato UGen is
    *                         running at audio rate, this must be an audio-rate input
    *                         as well.
    * @param beat             vibrato rate, that is the speed of wobble in Hertz.
    *                         Note that if this is set to a too low value, you may
    *                         never get vibrato back, since this input is only checked
    *                         at the end of a cycle.
    * @param depth            amount of vibrato frequency deviation around the
    *                         fundamental, as a proportion of the fundamental.
    * @param delay            delay in seconds before vibrato is established.
    *                         ''(init-time only)''
    * @param onset            transition time in seconds from no vibrato to full
    *                         vibrato after the initial delay time. ''(init-time
    *                         only)''
    * @param beatVar          random (noise) variation on the beat parameter,
    *                         expressed as a proportion of `beat` . It can change once
    *                         per cycle of vibrato.
    * @param depthVar         random (noise) variation on the depth of modulation,
    *                         expressed as a proportion of `depth` . It can change
    *                         once per cycle of vibrato. The noise affects
    *                         independently the up and the down part of vibrato shape
    *                         within a cycle.
    * @param iphase           initial phase of vibrato modulation, allowing starting
    *                         above or below the fundamental rather than on it.
    *                         ''(init-time only)''
    */
  def kr(freq: GE = 440.0f, beat: GE = 6.0f, depth: GE = 0.02f, delay: GE = 0.0f, onset: GE = 0.0f, beatVar: GE = 0.04f, depthVar: GE = 0.1f, iphase: GE = 0.0f): Vibrato = 
    new Vibrato(control, freq, beat, depth, delay, onset, beatVar, depthVar, iphase)
  
  def ar: Vibrato = ar()
  
  /** @param freq             fundamental frequency in Hertz. If the Vibrato UGen is
    *                         running at audio rate, this must be an audio-rate input
    *                         as well.
    * @param beat             vibrato rate, that is the speed of wobble in Hertz.
    *                         Note that if this is set to a too low value, you may
    *                         never get vibrato back, since this input is only checked
    *                         at the end of a cycle.
    * @param depth            amount of vibrato frequency deviation around the
    *                         fundamental, as a proportion of the fundamental.
    * @param delay            delay in seconds before vibrato is established.
    *                         ''(init-time only)''
    * @param onset            transition time in seconds from no vibrato to full
    *                         vibrato after the initial delay time. ''(init-time
    *                         only)''
    * @param beatVar          random (noise) variation on the beat parameter,
    *                         expressed as a proportion of `beat` . It can change once
    *                         per cycle of vibrato.
    * @param depthVar         random (noise) variation on the depth of modulation,
    *                         expressed as a proportion of `depth` . It can change
    *                         once per cycle of vibrato. The noise affects
    *                         independently the up and the down part of vibrato shape
    *                         within a cycle.
    * @param iphase           initial phase of vibrato modulation, allowing starting
    *                         above or below the fundamental rather than on it.
    *                         ''(init-time only)''
    */
  def ar(freq: GE = 440.0f, beat: GE = 6.0f, depth: GE = 0.02f, delay: GE = 0.0f, onset: GE = 0.0f, beatVar: GE = 0.04f, depthVar: GE = 0.1f, iphase: GE = 0.0f): Vibrato = 
    new Vibrato(audio, freq, beat, depth, delay, onset, beatVar, depthVar, iphase)
  
  def read(in: RefMapIn, arity: Int): Vibrato = {
    require (arity == 9)
    val _rate     = in.readRate()
    val _freq     = in.readGE()
    val _beat     = in.readGE()
    val _depth    = in.readGE()
    val _delay    = in.readGE()
    val _onset    = in.readGE()
    val _beatVar  = in.readGE()
    val _depthVar = in.readGE()
    val _iphase   = in.readGE()
    new Vibrato(_rate, _freq, _beat, _depth, _delay, _onset, _beatVar, _depthVar, _iphase)
  }
}

/** A low frequency oscillator UGen for modelling vibrato. It produces a modulating
  * frequency value in Hertz that can be used as the frequency parameter of another
  * UGen.
  * 
  * By setting more extreme settings, one can get back to the timbres of FM
  * synthesis. One can also add in some noise to the vibrato rate and vibrato size
  * (modulation depth) to make for a more realistic motor pattern.
  * 
  * The vibrato output is a waveform based on a squared envelope shape with four
  * stages marking out 0.0 to 1.0, 1.0 to 0.0, 0.0 to -1.0, and -1.0 back to 0.0.
  * The vibrato rate determines how quickly one moves through these stages.
  * 
  * @param freq             fundamental frequency in Hertz. If the Vibrato UGen is
  *                         running at audio rate, this must be an audio-rate input
  *                         as well.
  * @param beat             vibrato rate, that is the speed of wobble in Hertz.
  *                         Note that if this is set to a too low value, you may
  *                         never get vibrato back, since this input is only checked
  *                         at the end of a cycle.
  * @param depth            amount of vibrato frequency deviation around the
  *                         fundamental, as a proportion of the fundamental.
  * @param delay            delay in seconds before vibrato is established.
  *                         ''(init-time only)''
  * @param onset            transition time in seconds from no vibrato to full
  *                         vibrato after the initial delay time. ''(init-time
  *                         only)''
  * @param beatVar          random (noise) variation on the beat parameter,
  *                         expressed as a proportion of `beat` . It can change once
  *                         per cycle of vibrato.
  * @param depthVar         random (noise) variation on the depth of modulation,
  *                         expressed as a proportion of `depth` . It can change
  *                         once per cycle of vibrato. The noise affects
  *                         independently the up and the down part of vibrato shape
  *                         within a cycle.
  * @param iphase           initial phase of vibrato modulation, allowing starting
  *                         above or below the fundamental rather than on it.
  *                         ''(init-time only)''
  */
final case class Vibrato(rate: Rate, freq: GE = 440.0f, beat: GE = 6.0f, depth: GE = 0.02f, delay: GE = 0.0f, onset: GE = 0.0f, beatVar: GE = 0.04f, depthVar: GE = 0.1f, iphase: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, beat.expand, depth.expand, delay.expand, onset.expand, beatVar.expand, depthVar.expand, iphase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A non-band-limited pulse oscillator UGen. Outputs a high value of one and a low
  * value of zero.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulating frequency
  * play { LFPulse.ar(XLine.kr(1, 200, 10), 0, 0.2) * 0.1 }
  * }}}
  * {{{
  * // modulating amplitude
  * play { LFPulse.kr(XLine.kr(1, 200, 10), 0, 0.2) * SinOsc.ar(440) * 0.1 }
  * }}}
  * {{{
  * // used as both oscillator and LFO
  * play { LFPulse.ar(LFPulse.kr(3, 0, 0.3).mulAdd(200, 200), 0, 0.2) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Pulse$ Pulse]]
  */
object LFPulse extends ProductReader[LFPulse] {
  def kr: LFPulse = kr()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase offset in cycles ( `0..1` ). If you think
    *                         of a buffer of one cycle of the waveform, this is the
    *                         starting offset into this buffer. Hence, an `iphase` of
    *                         `0.25` means that you will hear the first impulse after
    *                         `0.75` periods! If you prefer to specify the perceived
    *                         delay instead, you could use an `iphase` of `-0.25 + 1`
    *                         which is more intuitive. Note that the phase is not
    *                         automatically wrapped into the range of `0..1` , so
    *                         putting an `iphase` of `-0.25` currently results in a
    *                         strange initial signal which only stabilizes to the
    *                         correct behaviour after one period! ''(init-time only)''
    * @param width            pulse width duty cycle from zero to one. If you want to
    *                         specify the width rather in seconds, you can use the
    *                         formula `width = freq * dur` , e.g. for a single sample
    *                         impulse use `width = freq * SampleDur.ir` .
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f, width: GE = 0.5f): LFPulse = 
    new LFPulse(control, freq, iphase, width)
  
  def ar: LFPulse = ar()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase offset in cycles ( `0..1` ). If you think
    *                         of a buffer of one cycle of the waveform, this is the
    *                         starting offset into this buffer. Hence, an `iphase` of
    *                         `0.25` means that you will hear the first impulse after
    *                         `0.75` periods! If you prefer to specify the perceived
    *                         delay instead, you could use an `iphase` of `-0.25 + 1`
    *                         which is more intuitive. Note that the phase is not
    *                         automatically wrapped into the range of `0..1` , so
    *                         putting an `iphase` of `-0.25` currently results in a
    *                         strange initial signal which only stabilizes to the
    *                         correct behaviour after one period! ''(init-time only)''
    * @param width            pulse width duty cycle from zero to one. If you want to
    *                         specify the width rather in seconds, you can use the
    *                         formula `width = freq * dur` , e.g. for a single sample
    *                         impulse use `width = freq * SampleDur.ir` .
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f, width: GE = 0.5f): LFPulse = 
    new LFPulse(audio, freq, iphase, width)
  
  def read(in: RefMapIn, arity: Int): LFPulse = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _iphase = in.readGE()
    val _width  = in.readGE()
    new LFPulse(_rate, _freq, _iphase, _width)
  }
}

/** A non-band-limited pulse oscillator UGen. Outputs a high value of one and a low
  * value of zero.
  * 
  * @param freq             oscillator frequency in Hertz
  * @param iphase           initial phase offset in cycles ( `0..1` ). If you think
  *                         of a buffer of one cycle of the waveform, this is the
  *                         starting offset into this buffer. Hence, an `iphase` of
  *                         `0.25` means that you will hear the first impulse after
  *                         `0.75` periods! If you prefer to specify the perceived
  *                         delay instead, you could use an `iphase` of `-0.25 + 1`
  *                         which is more intuitive. Note that the phase is not
  *                         automatically wrapped into the range of `0..1` , so
  *                         putting an `iphase` of `-0.25` currently results in a
  *                         strange initial signal which only stabilizes to the
  *                         correct behaviour after one period! ''(init-time only)''
  * @param width            pulse width duty cycle from zero to one. If you want to
  *                         specify the width rather in seconds, you can use the
  *                         formula `width = freq * dur` , e.g. for a single sample
  *                         impulse use `width = freq * SampleDur.ir` .
  * 
  * @see [[de.sciss.synth.ugen.Pulse$ Pulse]]
  */
final case class LFPulse(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f, width: GE = 0.5f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand, width.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sawtooth oscillator UGen. The oscillator is creating an aliased sawtooth,
  * that is it does not use band-limiting. For a band-limited version use `Saw`
  * instead. The signal range is -1 to +1.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulating frequency
  * play { LFSaw.ar(XLine.kr(1, 200, 10)) * 0.1 }
  * }}}
  * {{{
  * // modulating amplitude
  * play { LFSaw.kr(XLine.kr(1, 200, 10)) * SinOsc.ar(440) * 0.1 }
  * }}}
  * {{{
  * // neuer deutscher Sägezahn
  * play { LFSaw.ar(LFSaw.kr(3).mulAdd(200, 200)) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Saw$ Saw]]
  */
object LFSaw extends ProductReader[LFSaw] {
  def kr: LFSaw = kr()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase offset. For efficiency reasons this is a
    *                         value ranging from -1 to 1 (thus equal to the initial
    *                         output value). Note that a phase of zero (default) means
    *                         the wave starts at 0 and rises to +1 before jumping down
    *                         to -1. Use a phase of 1 to have the wave start at -1.
    *                         ''(init-time only)''
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f): LFSaw = new LFSaw(control, freq, iphase)
  
  def ar: LFSaw = ar()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase offset. For efficiency reasons this is a
    *                         value ranging from -1 to 1 (thus equal to the initial
    *                         output value). Note that a phase of zero (default) means
    *                         the wave starts at 0 and rises to +1 before jumping down
    *                         to -1. Use a phase of 1 to have the wave start at -1.
    *                         ''(init-time only)''
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f): LFSaw = new LFSaw(audio, freq, iphase)
  
  def read(in: RefMapIn, arity: Int): LFSaw = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _iphase = in.readGE()
    new LFSaw(_rate, _freq, _iphase)
  }
}

/** A sawtooth oscillator UGen. The oscillator is creating an aliased sawtooth,
  * that is it does not use band-limiting. For a band-limited version use `Saw`
  * instead. The signal range is -1 to +1.
  * 
  * @param freq             oscillator frequency in Hertz
  * @param iphase           initial phase offset. For efficiency reasons this is a
  *                         value ranging from -1 to 1 (thus equal to the initial
  *                         output value). Note that a phase of zero (default) means
  *                         the wave starts at 0 and rises to +1 before jumping down
  *                         to -1. Use a phase of 1 to have the wave start at -1.
  *                         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.Saw$ Saw]]
  */
final case class LFSaw(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sine-like oscillator UGen with a shape made of two parabolas. It has audible
  * odd harmonics and is non-band-limited. Its output ranges from -1 to +1.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulating frequency
  * play { LFPar.ar(XLine.kr(100, 20000, 10)) * 0.1 }
  * }}}
  * {{{
  * // modulating amplitude
  * play { LFPar.kr(XLine.kr(1, 200, 10)) * SinOsc.ar(440) * 0.1 }
  * }}}
  * {{{
  * // used as both oscillator and LFO
  * play { LFPar.ar(LFPar.kr(LFPar.kr(0.2).mulAdd(8,10)).mulAdd(400,800)) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFCub$ LFCub]]
  */
object LFPar extends ProductReader[LFPar] {
  def kr: LFPar = kr()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase in cycle (0 to 1) ''(init-time only)''
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f): LFPar = new LFPar(control, freq, iphase)
  
  def ar: LFPar = ar()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase in cycle (0 to 1) ''(init-time only)''
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f): LFPar = new LFPar(audio, freq, iphase)
  
  def read(in: RefMapIn, arity: Int): LFPar = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _iphase = in.readGE()
    new LFPar(_rate, _freq, _iphase)
  }
}

/** A sine-like oscillator UGen with a shape made of two parabolas. It has audible
  * odd harmonics and is non-band-limited. Its output ranges from -1 to +1.
  * 
  * @param freq             oscillator frequency in Hertz
  * @param iphase           initial phase in cycle (0 to 1) ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.LFCub$ LFCub]]
  */
final case class LFPar(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sine-like oscillator UGen with a shape made of two cubic pieces. It is
  * smoother than `LFPar` .
  * 
  * ===Examples===
  * 
  * {{{
  * // modulating frequency
  * play { LFPar.ar(XLine.kr(100, 20000, 10)) * 0.1 }
  * }}}
  * {{{
  * // modulating amplitude
  * play { LFPar.kr(XLine.kr(1, 200, 10)) * SinOsc.ar(440) * 0.1 }
  * }}}
  * {{{
  * // used as both oscillator and LFO
  * play { LFPar.ar(LFPar.kr(LFPar.kr(0.2).mulAdd(8,10)).mulAdd(400,800)) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFPar$ LFPar]]
  */
object LFCub extends ProductReader[LFCub] {
  def kr: LFCub = kr()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase in cycle (0 to 1) ''(init-time only)''
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f): LFCub = new LFCub(control, freq, iphase)
  
  def ar: LFCub = ar()
  
  /** @param freq             oscillator frequency in Hertz
    * @param iphase           initial phase in cycle (0 to 1) ''(init-time only)''
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f): LFCub = new LFCub(audio, freq, iphase)
  
  def read(in: RefMapIn, arity: Int): LFCub = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _iphase = in.readGE()
    new LFCub(_rate, _freq, _iphase)
  }
}

/** A sine-like oscillator UGen with a shape made of two cubic pieces. It is
  * smoother than `LFPar` .
  * 
  * @param freq             oscillator frequency in Hertz
  * @param iphase           initial phase in cycle (0 to 1) ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.LFPar$ LFPar]]
  */
final case class LFCub(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A triangle oscillator UGen designed for low frequency control signals (being
  * non-band-limited). The output varies from -1 to 1.
  * 
  * With an initial phase of zero, the oscillator begins at 0, rises to 1, then
  * falls to -1 and goes back to zero after one complete phase. With an initial
  * phase of 1 (corresponding to 90 degrees), the oscillator begins at 1 and then
  * falls to -1. With an initial phase of 3 (or 270 degrees), the oscillator begins
  * at -1 and then rises to 1.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulating frequency
  * play { LFTri.ar(XLine.kr(100, 20000, 10)) * 0.1 }
  * }}}
  * {{{
  * // modulating amplitude
  * play { LFTri.kr(XLine.kr(1, 200, 10)) * SinOsc.ar(440) * 0.1 }
  * }}}
  * {{{
  * // used as both oscillator and LFO
  * play { LFTri.ar(LFTri.kr(LFTri.kr(0.2).mulAdd(8,10)).mulAdd(400,800)) * 0.1 }
  * }}}
  */
object LFTri extends ProductReader[LFTri] {
  def kr: LFTri = kr()
  
  /** @param freq             frequency in Hertz
    * @param iphase           initial phase of the oscillator. A full phase (2 Pi or
    *                         360 degrees) is represented by an `iphase` value of 4.
    *                         The initial phase cannot be modulated. ''(init-time
    *                         only)''
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f): LFTri = new LFTri(control, freq, iphase)
  
  def ar: LFTri = ar()
  
  /** @param freq             frequency in Hertz
    * @param iphase           initial phase of the oscillator. A full phase (2 Pi or
    *                         360 degrees) is represented by an `iphase` value of 4.
    *                         The initial phase cannot be modulated. ''(init-time
    *                         only)''
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f): LFTri = new LFTri(audio, freq, iphase)
  
  def read(in: RefMapIn, arity: Int): LFTri = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _iphase = in.readGE()
    new LFTri(_rate, _freq, _iphase)
  }
}

/** A triangle oscillator UGen designed for low frequency control signals (being
  * non-band-limited). The output varies from -1 to 1.
  * 
  * With an initial phase of zero, the oscillator begins at 0, rises to 1, then
  * falls to -1 and goes back to zero after one complete phase. With an initial
  * phase of 1 (corresponding to 90 degrees), the oscillator begins at 1 and then
  * falls to -1. With an initial phase of 3 (or 270 degrees), the oscillator begins
  * at -1 and then rises to 1.
  * 
  * @param freq             frequency in Hertz
  * @param iphase           initial phase of the oscillator. A full phase (2 Pi or
  *                         360 degrees) is represented by an `iphase` value of 4.
  *                         The initial phase cannot be modulated. ''(init-time
  *                         only)''
  */
final case class LFTri(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-band-limited gaussian function oscillator UGen. Output ranges from
  * `minVal` to 1. It implements the formula:
  * {{{
  * f(x) = exp((x - phase).squared / (-2 * width.squared))
  * }}}
  * where `x` is to vary in the range -1 to 1 over the period `dur` . `minVal` is
  * the initial value at -1. E.g. for default parameters, it is `exp(-50)` or
  * roughly zero.
  * 
  * ===Examples===
  * 
  * {{{
  * // dur and width can be modulated at audio rate
  * play {
  *   val dur = SinOsc.ar(MouseX.kr(2, 1000, 1) * Seq(1.0, 1.1)).linLin(-1, 1, 0.0006, 0.01)
  *   val width = SinOsc.ar(Seq(0.5, 0.55)).linLin(-1, 1, 0.01, 0.3)
  *   LFGauss.ar(dur, width) * 0.2
  * }
  * }}}
  * {{{
  * // several frequencies and widths combined
  * play {
  *   val x    = MouseX.kr(1, 0.07, 1)
  *   val y    = MouseY.kr(1, 3)
  *   val mod  = LFGauss.ar(x, (-1 to -6 by -1).map(i => y.pow(i)))
  *   val carr = SinOsc.ar((0 to 5).map(i => 200 * 1.3.pow(i)))
  *   Mix(carr * mod) * 0.1
  * }
  * }}}
  * {{{
  * // test spectrum
  * play {
  *   val son = LeakDC.ar(LFGauss.ar(0.005, 0.2))
  *   BPF.ar(son * 3, MouseX.kr(60, 2000, 1), 0.05)
  * }
  * }}}
  */
object LFGauss extends ProductReader[LFGauss] {
  def kr: LFGauss = kr()
  
  /** @param dur              duration in seconds of a full -1 <= x <= 1 cycle, or
    *                         the reciprocal of the frequency
    * @param width            relative width of the bell. Best to keep below 0.25
    *                         when used as envelope.
    * @param phase            phase offset
    * @param loop             if greater than zero, the UGen oscillates. Otherwise it
    *                         calls `doneAction` after one cycle.
    * @param doneAction       evaluated after cycle completes
    */
  def kr(dur: GE = 1.0f, width: GE = 0.1f, phase: GE = 0.0f, loop: GE = 1, doneAction: GE = doNothing): LFGauss = 
    new LFGauss(control, dur, width, phase, loop, doneAction)
  
  def ar: LFGauss = ar()
  
  /** @param dur              duration in seconds of a full -1 <= x <= 1 cycle, or
    *                         the reciprocal of the frequency
    * @param width            relative width of the bell. Best to keep below 0.25
    *                         when used as envelope.
    * @param phase            phase offset
    * @param loop             if greater than zero, the UGen oscillates. Otherwise it
    *                         calls `doneAction` after one cycle.
    * @param doneAction       evaluated after cycle completes
    */
  def ar(dur: GE = 1.0f, width: GE = 0.1f, phase: GE = 0.0f, loop: GE = 1, doneAction: GE = doNothing): LFGauss = 
    new LFGauss(audio, dur, width, phase, loop, doneAction)
  
  def read(in: RefMapIn, arity: Int): LFGauss = {
    require (arity == 6)
    val _rate       = in.readRate()
    val _dur        = in.readGE()
    val _width      = in.readGE()
    val _phase      = in.readGE()
    val _loop       = in.readGE()
    val _doneAction = in.readGE()
    new LFGauss(_rate, _dur, _width, _phase, _loop, _doneAction)
  }
}

/** A non-band-limited gaussian function oscillator UGen. Output ranges from
  * `minVal` to 1. It implements the formula:
  * {{{
  * f(x) = exp((x - phase).squared / (-2 * width.squared))
  * }}}
  * where `x` is to vary in the range -1 to 1 over the period `dur` . `minVal` is
  * the initial value at -1. E.g. for default parameters, it is `exp(-50)` or
  * roughly zero.
  * 
  * @param dur              duration in seconds of a full -1 <= x <= 1 cycle, or
  *                         the reciprocal of the frequency
  * @param width            relative width of the bell. Best to keep below 0.25
  *                         when used as envelope.
  * @param phase            phase offset
  * @param loop             if greater than zero, the UGen oscillates. Otherwise it
  *                         calls `doneAction` after one cycle.
  * @param doneAction       evaluated after cycle completes
  */
final case class LFGauss(rate: Rate, dur: GE = 1.0f, width: GE = 0.1f, phase: GE = 0.0f, loop: GE = 1, doneAction: GE = doNothing)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](dur.expand, width.expand, phase.expand, loop.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-band-limited generator UGen for single sample impulses.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulating frequency
  * play { Impulse.ar(XLine.kr(100, 20000, 10)) * 0.3 }
  * }}}
  * {{{
  * // modulating amplitude
  * play { Impulse.kr(XLine.kr(1, 200, 10)) * SinOsc.ar(440) * 0.3 }
  * }}}
  * {{{
  * // modulating phase
  * play { Impulse.ar(4, Seq(DC.kr(0), MouseX.kr(0, 1))) * 0.3 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFPulse$ LFPulse]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.Blip$ Blip]]
  */
object Impulse extends ProductReader[Impulse] {
  def kr: Impulse = kr()
  
  /** @param freq             frequency in Hertz. A value of zero can be used to
    *                         generate a singular impulse.
    * @param phase            phase offset in cycles (0 to 1)
    */
  def kr(freq: GE = 440.0f, phase: GE = 0.0f): Impulse = new Impulse(control, freq, phase)
  
  def ar: Impulse = ar()
  
  /** @param freq             frequency in Hertz. A value of zero can be used to
    *                         generate a singular impulse.
    * @param phase            phase offset in cycles (0 to 1)
    */
  def ar(freq: GE = 440.0f, phase: GE = 0.0f): Impulse = new Impulse(audio, freq, phase)
  
  def read(in: RefMapIn, arity: Int): Impulse = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _phase  = in.readGE()
    new Impulse(_rate, _freq, _phase)
  }
}

/** A non-band-limited generator UGen for single sample impulses.
  * 
  * @param freq             frequency in Hertz. A value of zero can be used to
  *                         generate a singular impulse.
  * @param phase            phase offset in cycles (0 to 1)
  * 
  * @see [[de.sciss.synth.ugen.LFPulse$ LFPulse]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.Blip$ Blip]]
  */
final case class Impulse(rate: Rate, freq: GE = 440.0f, phase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, phase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sawtooth-triangle oscillator UGen with variable duty. A `width` of zero
  * produces a sawtooth of falling slope, with an initial phase of zero making it
  * start at +1. A `width` of 0.5 produces a triangle wave, starting at -1 then
  * raising to +1, then falling again to -1. A `width` of 1.0 produces a sawtooth of
  * rising slope, starting -1.
  * 
  * Increasing the initial wave will increase the offset into the waveform. For
  * example, with a phase of 0.5 and a width of 0.5, the result is a triangle
  * waveform that starts at +1.
  * 
  * There is a strange anomaly for the falling sawtooth (zero width): Instead of
  * starting directly at +1, the first sample is -1 and only from the second sample
  * at +1 the waveform starts falling. In other words, the waveform has a delay of
  * one sample.
  * 
  * ===Examples===
  * 
  * {{{
  * // width modulation
  * play {
  *   val freq  = LFPulse.kr(3, 0, 0.3).mulAdd(200, 200)
  *   val width = LFTri.kr(1.0).mulAdd(0.5, 0.5)
  *   VarSaw.ar(freq, 0, width) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Saw$ Saw]]
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  * @see [[de.sciss.synth.ugen.SyncSaw$ SyncSaw]]
  */
object VarSaw extends ProductReader[VarSaw] {
  def kr: VarSaw = kr()
  
  /** @param freq             frequency in Hertz
    * @param iphase           initial phase offset in cycle (0 to 1)
    * @param width            duty cycle from zero to one.
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f, width: GE = 0.5f): VarSaw = 
    new VarSaw(control, freq, iphase, width)
  
  def ar: VarSaw = ar()
  
  /** @param freq             frequency in Hertz
    * @param iphase           initial phase offset in cycle (0 to 1)
    * @param width            duty cycle from zero to one.
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f, width: GE = 0.5f): VarSaw = 
    new VarSaw(audio, freq, iphase, width)
  
  def read(in: RefMapIn, arity: Int): VarSaw = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _iphase = in.readGE()
    val _width  = in.readGE()
    new VarSaw(_rate, _freq, _iphase, _width)
  }
}

/** A sawtooth-triangle oscillator UGen with variable duty. A `width` of zero
  * produces a sawtooth of falling slope, with an initial phase of zero making it
  * start at +1. A `width` of 0.5 produces a triangle wave, starting at -1 then
  * raising to +1, then falling again to -1. A `width` of 1.0 produces a sawtooth of
  * rising slope, starting -1.
  * 
  * Increasing the initial wave will increase the offset into the waveform. For
  * example, with a phase of 0.5 and a width of 0.5, the result is a triangle
  * waveform that starts at +1.
  * 
  * There is a strange anomaly for the falling sawtooth (zero width): Instead of
  * starting directly at +1, the first sample is -1 and only from the second sample
  * at +1 the waveform starts falling. In other words, the waveform has a delay of
  * one sample.
  * 
  * @param freq             frequency in Hertz
  * @param iphase           initial phase offset in cycle (0 to 1)
  * @param width            duty cycle from zero to one.
  * 
  * @see [[de.sciss.synth.ugen.Saw$ Saw]]
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  * @see [[de.sciss.synth.ugen.SyncSaw$ SyncSaw]]
  */
final case class VarSaw(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f, width: GE = 0.5f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand, width.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sawtooth oscillator UGen that is hard sync'ed to a fundamental pitch. That
  * is, a sawtooth waveform is produced at one frequency, `sawFreq` , whereas a
  * trigger at a another frequency, `syncFreq` , resets the phase of the sawtooth to
  * zero.
  * 
  * This produces an effect similar to moving formants or pulse width modulation.
  * This is not a band limited waveform, so it may alias.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulate saw frequency
  * play { SyncSaw.ar(100, Line.kr(100, 800, 12)) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Saw$ Saw]]
  * @see [[de.sciss.synth.ugen.VarSaw$ VarSaw]]
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  */
object SyncSaw extends ProductReader[SyncSaw] {
  def kr: SyncSaw = kr()
  
  /** @param syncFreq         synchronizing (principal) frequency which is the
    *                         perceived fundamental
    * @param sawFreq          subordinate sawtooth frequency. It should typically be
    *                         greater than `syncFreq` .
    */
  def kr(syncFreq: GE = 440.0f, sawFreq: GE = 440.0f): SyncSaw = new SyncSaw(control, syncFreq, sawFreq)
  
  def ar: SyncSaw = ar()
  
  /** @param syncFreq         synchronizing (principal) frequency which is the
    *                         perceived fundamental
    * @param sawFreq          subordinate sawtooth frequency. It should typically be
    *                         greater than `syncFreq` .
    */
  def ar(syncFreq: GE = 440.0f, sawFreq: GE = 440.0f): SyncSaw = new SyncSaw(audio, syncFreq, sawFreq)
  
  def read(in: RefMapIn, arity: Int): SyncSaw = {
    require (arity == 3)
    val _rate     = in.readRate()
    val _syncFreq = in.readGE()
    val _sawFreq  = in.readGE()
    new SyncSaw(_rate, _syncFreq, _sawFreq)
  }
}

/** A sawtooth oscillator UGen that is hard sync'ed to a fundamental pitch. That
  * is, a sawtooth waveform is produced at one frequency, `sawFreq` , whereas a
  * trigger at a another frequency, `syncFreq` , resets the phase of the sawtooth to
  * zero.
  * 
  * This produces an effect similar to moving formants or pulse width modulation.
  * This is not a band limited waveform, so it may alias.
  * 
  * @param syncFreq         synchronizing (principal) frequency which is the
  *                         perceived fundamental
  * @param sawFreq          subordinate sawtooth frequency. It should typically be
  *                         greater than `syncFreq` .
  * 
  * @see [[de.sciss.synth.ugen.Saw$ Saw]]
  * @see [[de.sciss.synth.ugen.VarSaw$ VarSaw]]
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  */
final case class SyncSaw(rate: Rate, syncFreq: GE = 440.0f, sawFreq: GE = 440.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](syncFreq.expand, sawFreq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A converter UGen that takes a control-rate input and produces an audio-rate
  * output by means of linear interpolation. The current control input value is
  * always reached in at the beginning of the subsequent control block. A special
  * case is the initialization which begins directly at the first control input
  * value (therefore, the first control block of the audio-rate output is is always
  * constant.)
  * 
  * For example, if the block size is 64, and the first three input values are
  * -0.5, 0.6, 0.3, then the output signal will be 65 samples of value -0.5,
  * followed by a linear ramp of 64 samples towards 0.6, followed by a linear ramp
  * of 64 samples to towards 0.3.
  * 
  * ===Examples===
  * 
  * {{{
  * // compare control and audio rate
  * play {
  *   val a = K2A.ar(WhiteNoise.kr(0.3))
  *   val b =        WhiteNoise.ar(0.3)
  *   val c = LFPulse.ar(1, Seq(0, 0.5))
  *   c * Seq(a, b)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.A2K$ A2K]]
  * @see [[de.sciss.synth.ugen.T2A$ T2A]]
  * @see [[de.sciss.synth.ugen.DC$ DC]]
  */
object K2A extends ProductReader[K2A] {
  /** @param in               control-rate signal to convert
    */
  def ar(in: GE): K2A = new K2A(in)
  
  def read(in: RefMapIn, arity: Int): K2A = {
    require (arity == 1)
    val _in = in.readGE()
    new K2A(_in)
  }
}

/** A converter UGen that takes a control-rate input and produces an audio-rate
  * output by means of linear interpolation. The current control input value is
  * always reached in at the beginning of the subsequent control block. A special
  * case is the initialization which begins directly at the first control input
  * value (therefore, the first control block of the audio-rate output is is always
  * constant.)
  * 
  * For example, if the block size is 64, and the first three input values are
  * -0.5, 0.6, 0.3, then the output signal will be 65 samples of value -0.5,
  * followed by a linear ramp of 64 samples towards 0.6, followed by a linear ramp
  * of 64 samples to towards 0.3.
  * 
  * @param in               control-rate signal to convert
  * 
  * @see [[de.sciss.synth.ugen.A2K$ A2K]]
  * @see [[de.sciss.synth.ugen.T2A$ T2A]]
  * @see [[de.sciss.synth.ugen.DC$ DC]]
  */
final case class K2A(in: GE) extends UGenSource.SingleOut with AudioRated {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, audio, _args)
}

/** A converter UGen that takes an audio-rate input and produces a control-rate
  * output by means of sampling. The sample is always taken at the beginning of each
  * control-block, while all other samples of the audio-rate input within that block
  * are ignored.
  * 
  * @see [[de.sciss.synth.ugen.K2A$ K2A]]
  * @see [[de.sciss.synth.ugen.T2K$ T2K]]
  */
object A2K extends ProductReader[A2K] {
  /** @param in               audio-rate signal to convert
    */
  def kr(in: GE): A2K = new A2K(in)
  
  def read(in: RefMapIn, arity: Int): A2K = {
    require (arity == 1)
    val _in = in.readGE()
    new A2K(_in)
  }
}

/** A converter UGen that takes an audio-rate input and produces a control-rate
  * output by means of sampling. The sample is always taken at the beginning of each
  * control-block, while all other samples of the audio-rate input within that block
  * are ignored.
  * 
  * @param in               audio-rate signal to convert
  * 
  * @see [[de.sciss.synth.ugen.K2A$ K2A]]
  * @see [[de.sciss.synth.ugen.T2K$ T2K]]
  */
final case class A2K(in: GE) extends UGenSource.SingleOut with ControlRated {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, control, _args)
}

/** A UGen that converts an audio-rate trigger input into a control-rate trigger
  * output. A trigger occurs when a signal changes from less than or equal to zero
  * to greater than zero. The UGen behaves strangely in that for a rising slope
  * input signal, it will report the maximum value seen within the calculation
  * block, but if the slope extends to the next block, it will output that second's
  * block maximum value again instead of waiting for a fall to <= 0.
  * 
  * ===Examples===
  * 
  * {{{
  * // down-sample audio-rate dust
  * play {
  *   val trig = T2K.kr(Dust.ar(4))
  *   Trig.kr(trig, 0.1) * SinOsc.ar(800) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.K2A$ K2A]]
  * @see [[de.sciss.synth.ugen.T2K$ T2K]]
  * @see [[de.sciss.synth.ugen.Trig$ Trig]]
  */
object T2K extends ProductReader[T2K] {
  /** @param in               audio-rate trigger input
    */
  def kr(in: GE): T2K = new T2K(in)
  
  def read(in: RefMapIn, arity: Int): T2K = {
    require (arity == 1)
    val _in = in.readGE()
    new T2K(_in)
  }
}

/** A UGen that converts an audio-rate trigger input into a control-rate trigger
  * output. A trigger occurs when a signal changes from less than or equal to zero
  * to greater than zero. The UGen behaves strangely in that for a rising slope
  * input signal, it will report the maximum value seen within the calculation
  * block, but if the slope extends to the next block, it will output that second's
  * block maximum value again instead of waiting for a fall to <= 0.
  * 
  * @param in               audio-rate trigger input
  * 
  * @see [[de.sciss.synth.ugen.K2A$ K2A]]
  * @see [[de.sciss.synth.ugen.T2K$ T2K]]
  * @see [[de.sciss.synth.ugen.Trig$ Trig]]
  */
final case class T2K(in: GE) extends UGenSource.SingleOut with ControlRated {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRateT(_args, 0, audio)
    UGen.SingleOut(name, control, _args1)
  }
}

/** A UGen that converts a control-rate trigger input into an audio-rate trigger
  * output. A trigger occurs when a signal changes from less than or equal to zero
  * to greater than zero. The output will have a single sample spike of the input
  * trigger's amplitude at the beginning of the calculation block.
  * 
  * ===Examples===
  * 
  * {{{
  * // up-sample control-rate impulses
  * play {
  *   val trig = Impulse.kr(MouseX.kr(1, 100, 1))
  *   Ringz.ar(T2A.ar(trig), 800, 0.01) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.K2A$ K2A]]
  * @see [[de.sciss.synth.ugen.T2K$ T2K]]
  * @see [[de.sciss.synth.ugen.Trig$ Trig]]
  */
object T2A extends ProductReader[T2A] {
  /** @param in               control-rate trigger input
    */
  def ar(in: GE): T2A = new T2A(in)
  
  def read(in: RefMapIn, arity: Int): T2A = {
    require (arity == 1)
    val _in = in.readGE()
    new T2A(_in)
  }
}

/** A UGen that converts a control-rate trigger input into an audio-rate trigger
  * output. A trigger occurs when a signal changes from less than or equal to zero
  * to greater than zero. The output will have a single sample spike of the input
  * trigger's amplitude at the beginning of the calculation block.
  * 
  * @param in               control-rate trigger input
  * 
  * @see [[de.sciss.synth.ugen.K2A$ K2A]]
  * @see [[de.sciss.synth.ugen.T2K$ T2K]]
  * @see [[de.sciss.synth.ugen.Trig$ Trig]]
  */
final case class T2A(in: GE) extends UGenSource.SingleOut with AudioRated {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, audio, _args)
}

/** A UGen that creates a constant signal at a given calculation rate.
  * 
  * ===Examples===
  * 
  * {{{
  * // create a silent audio signal
  * play {
  *   // Note: Select.ar requires audio-rate input.
  *   // Therefore, DC can be used to wrap the otherwise
  *   // incompatible constant zero. In future versions of
  *   // ScalaCollider, this wrapping will be done
  *   // automatically, however.
  *   Select.ar(MouseButton.kr(lag = 0), Seq(DC.ar(0), SinOsc.ar * 0.2))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LeakDC$ LeakDC]]
  */
object DC extends ProductReader[DC] {
  /** @param in               constant value to output, fixed at initialisation time.
    *                         ''(init-time only)''
    */
  def kr(in: GE): DC = new DC(control, in)
  
  /** @param in               constant value to output, fixed at initialisation time.
    *                         ''(init-time only)''
    */
  def ar(in: GE): DC = new DC(audio, in)
  
  def read(in: RefMapIn, arity: Int): DC = {
    require (arity == 2)
    val _rate = in.readRate()
    val _in   = in.readGE()
    new DC(_rate, _in)
  }
}

/** A UGen that creates a constant signal at a given calculation rate.
  * 
  * @param in               constant value to output, fixed at initialisation time.
  *                         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.LeakDC$ LeakDC]]
  */
final case class DC(rate: Rate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A line generator UGen that moves from a start value to the end value in a given
  * duration.
  * 
  * ===Examples===
  * 
  * {{{
  * // pan from left to right
  * play { Pan2.ar(PinkNoise.ar(0.3), Line.kr(-1, 1, 10, freeSelf)) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.XLine$ XLine]]
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
object Line extends ProductReader[Line] {
  def kr: Line = kr()
  
  /** @param start            Starting value ''(init-time only)''
    * @param end              Ending value ''(init-time only)''
    * @param dur              Duration in seconds ''(init-time only)''
    * @param doneAction       A done-action that is evaluated when the Line has
    *                         reached the end value after the given duration
    */
  def kr(start: GE = 0.0f, end: GE = 1.0f, dur: GE = 1.0f, doneAction: GE = doNothing): Line = 
    new Line(control, start, end, dur, doneAction)
  
  def ar: Line = ar()
  
  /** @param start            Starting value ''(init-time only)''
    * @param end              Ending value ''(init-time only)''
    * @param dur              Duration in seconds ''(init-time only)''
    * @param doneAction       A done-action that is evaluated when the Line has
    *                         reached the end value after the given duration
    */
  def ar(start: GE = 0.0f, end: GE = 1.0f, dur: GE = 1.0f, doneAction: GE = doNothing): Line = 
    new Line(audio, start, end, dur, doneAction)
  
  def read(in: RefMapIn, arity: Int): Line = {
    require (arity == 5)
    val _rate       = in.readRate()
    val _start      = in.readGE()
    val _end        = in.readGE()
    val _dur        = in.readGE()
    val _doneAction = in.readGE()
    new Line(_rate, _start, _end, _dur, _doneAction)
  }
}

/** A line generator UGen that moves from a start value to the end value in a given
  * duration.
  * 
  * @param start            Starting value ''(init-time only)''
  * @param end              Ending value ''(init-time only)''
  * @param dur              Duration in seconds ''(init-time only)''
  * @param doneAction       A done-action that is evaluated when the Line has
  *                         reached the end value after the given duration
  * 
  * @see [[de.sciss.synth.ugen.XLine$ XLine]]
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
final case class Line(rate: Rate, start: GE = 0.0f, end: GE = 1.0f, dur: GE = 1.0f, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](start.expand, end.expand, dur.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** An exponential curve generator UGen that moves from a start value to the end
  * value in a given duration.
  * 
  * At a given point in time `0 <= t <= dur` , the output value is
  * `start * (stop/start).pow(t/dur)` .
  * 
  * '''Warning''': It must be ensured that the both `start` is not zero and `start`
  * and `end` have the same sign (e.g. a `start` of `-1` and an end of `-0.001` are
  * valid), otherwise the UGen will output a `NaN` ! While in the case of `end`
  * being zero the UGen will also output zero, it is recommended to treat this case
  * as pathological as well.
  * 
  * ===Examples===
  * 
  * {{{
  * // glissando
  * play { SinOsc.ar(Line.kr(200, 2000, 10, freeSelf)) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  */
object XLine extends ProductReader[XLine] {
  def kr: XLine = kr()
  
  /** @param start            Starting value ''(init-time only)''
    * @param end              Ending value ''(init-time only)''
    * @param dur              Duration in seconds ''(init-time only)''
    * @param doneAction       A done-action that is evaluated when the `Line` has
    *                         reached the end value after the given duration
    */
  def kr(start: GE = 1.0f, end: GE = 2.0f, dur: GE = 1.0f, doneAction: GE = doNothing): XLine = 
    new XLine(control, start, end, dur, doneAction)
  
  def ar: XLine = ar()
  
  /** @param start            Starting value ''(init-time only)''
    * @param end              Ending value ''(init-time only)''
    * @param dur              Duration in seconds ''(init-time only)''
    * @param doneAction       A done-action that is evaluated when the `Line` has
    *                         reached the end value after the given duration
    */
  def ar(start: GE = 1.0f, end: GE = 2.0f, dur: GE = 1.0f, doneAction: GE = doNothing): XLine = 
    new XLine(audio, start, end, dur, doneAction)
  
  def read(in: RefMapIn, arity: Int): XLine = {
    require (arity == 5)
    val _rate       = in.readRate()
    val _start      = in.readGE()
    val _end        = in.readGE()
    val _dur        = in.readGE()
    val _doneAction = in.readGE()
    new XLine(_rate, _start, _end, _dur, _doneAction)
  }
}

/** An exponential curve generator UGen that moves from a start value to the end
  * value in a given duration.
  * 
  * At a given point in time `0 <= t <= dur` , the output value is
  * `start * (stop/start).pow(t/dur)` .
  * 
  * '''Warning''': It must be ensured that the both `start` is not zero and `start`
  * and `end` have the same sign (e.g. a `start` of `-1` and an end of `-0.001` are
  * valid), otherwise the UGen will output a `NaN` ! While in the case of `end`
  * being zero the UGen will also output zero, it is recommended to treat this case
  * as pathological as well.
  * 
  * @param start            Starting value ''(init-time only)''
  * @param end              Ending value ''(init-time only)''
  * @param dur              Duration in seconds ''(init-time only)''
  * @param doneAction       A done-action that is evaluated when the `Line` has
  *                         reached the end value after the given duration
  * 
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  */
final case class XLine(rate: Rate, start: GE = 1.0f, end: GE = 2.0f, dur: GE = 1.0f, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](start.expand, end.expand, dur.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** A UGen that constrains a signal to a given range, by "wrapping" values outside
  * the range. This is similar to the `wrap2` binary operator but permits both a
  * lower range value `lo` and an upper range value `hi` .
  * 
  * An input value greater than or equal to `hi` will be wrapped back to
  * `(in - hi) % (hi - lo) + lo` . An input value less than `lo` will be wrapped
  * back to `hi - (lo - in) % (hi - lo)` .
  * 
  * ===Examples===
  * 
  * {{{
  * // wrap pulse wave to modulate timbre
  * play {
  *   val hi = SinOsc.ar(0.1).linExp(-1, 1, 0.01, 1.0)
  *   Wrap.ar(Pulse.ar(300), 0, hi) * 0.2 / hi
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Fold$ Fold]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
object Wrap extends ProductReader[Wrap] {
  /** @param in               input signal to constrain
    * @param lo               lower margin of wrapping (inclusive)
    * @param hi               upper margin of wrapping (exclusive)
    */
  def ir(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Wrap = new Wrap(scalar, in, lo, hi)
  
  /** @param in               input signal to constrain
    * @param lo               lower margin of wrapping (inclusive)
    * @param hi               upper margin of wrapping (exclusive)
    */
  def kr(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Wrap = new Wrap(control, in, lo, hi)
  
  /** @param in               input signal to constrain
    * @param lo               lower margin of wrapping (inclusive)
    * @param hi               upper margin of wrapping (exclusive)
    */
  def ar(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Wrap = new Wrap(audio, in, lo, hi)
  
  def read(in: RefMapIn, arity: Int): Wrap = {
    require (arity == 4)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    new Wrap(_rate, _in, _lo, _hi)
  }
}

/** A UGen that constrains a signal to a given range, by "wrapping" values outside
  * the range. This is similar to the `wrap2` binary operator but permits both a
  * lower range value `lo` and an upper range value `hi` .
  * 
  * An input value greater than or equal to `hi` will be wrapped back to
  * `(in - hi) % (hi - lo) + lo` . An input value less than `lo` will be wrapped
  * back to `hi - (lo - in) % (hi - lo)` .
  * 
  * @param in               input signal to constrain
  * @param lo               lower margin of wrapping (inclusive)
  * @param hi               upper margin of wrapping (exclusive)
  * 
  * @see [[de.sciss.synth.ugen.Fold$ Fold]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
final case class Wrap(rate: Rate, in: GE, lo: GE = 0.0f, hi: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that constrains a signal to a given range, by "folding" values outside
  * the range. This is similar to the `fold2` binary operator but permits both a
  * lower range value `lo` and an upper range value `hi` .
  * 
  * Folding can be understood as "reflecting" around the boundaries. For example,
  * if the upper margin is 3, then if an input value exceeds 3, the excess is
  * negatively reflected; 3.1 becomes 2.9, 3.2 becomes 2.8, etc. until the lower
  * margin is reached again where another reflection occurs. Likewise, if the lower
  * margin is 1, then if an input value falls below 1, the undershoot is reflected;
  * 0.9 becomes 1.1, 0.8 becomes 1.2, etc. until the upper margin is reached again
  * where another reflection occurs.
  * 
  * ===Examples===
  * 
  * {{{
  * // fold sawtooth wave to modulate timbre
  * play {
  *   val hi = SinOsc.ar(0.1).linExp(-1, 1, 0.01, 1.0)
  *   Fold.ar(Saw.ar(300), 0, hi) * 0.2 / hi
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Wrap$ Wrap]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
object Fold extends ProductReader[Fold] {
  /** @param in               input signal to constrain
    * @param lo               lower margin of folding
    * @param hi               upper margin of folding
    */
  def ir(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Fold = new Fold(scalar, in, lo, hi)
  
  /** @param in               input signal to constrain
    * @param lo               lower margin of folding
    * @param hi               upper margin of folding
    */
  def kr(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Fold = new Fold(control, in, lo, hi)
  
  /** @param in               input signal to constrain
    * @param lo               lower margin of folding
    * @param hi               upper margin of folding
    */
  def ar(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Fold = new Fold(audio, in, lo, hi)
  
  def read(in: RefMapIn, arity: Int): Fold = {
    require (arity == 4)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    new Fold(_rate, _in, _lo, _hi)
  }
}

/** A UGen that constrains a signal to a given range, by "folding" values outside
  * the range. This is similar to the `fold2` binary operator but permits both a
  * lower range value `lo` and an upper range value `hi` .
  * 
  * Folding can be understood as "reflecting" around the boundaries. For example,
  * if the upper margin is 3, then if an input value exceeds 3, the excess is
  * negatively reflected; 3.1 becomes 2.9, 3.2 becomes 2.8, etc. until the lower
  * margin is reached again where another reflection occurs. Likewise, if the lower
  * margin is 1, then if an input value falls below 1, the undershoot is reflected;
  * 0.9 becomes 1.1, 0.8 becomes 1.2, etc. until the upper margin is reached again
  * where another reflection occurs.
  * 
  * @param in               input signal to constrain
  * @param lo               lower margin of folding
  * @param hi               upper margin of folding
  * 
  * @see [[de.sciss.synth.ugen.Wrap$ Wrap]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
final case class Fold(rate: Rate, in: GE, lo: GE = 0.0f, hi: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that constrains a signal to a given range, by limiting values outside
  * the range to the range margins. This is similar to the `clip2` binary operator
  * but permits both a lower range value `lo` and an upper range value `hi` .
  * 
  * Mathematically, this is equivalent to `in.max(lo).min(hi)`.
  * 
  * Be aware that there seems to be an initialization bug. The following crashes,
  * indicating that `Clip.ar` outputs a zero initially:
  * 
  * {{ play { val bar = Integrator.ar(DC.ar(0), coeff = 0.999) val foo =
  * Clip.ar(bar, lo = 1.0, hi = 44100.0) // .max(1.0) val sum =
  * RunningSum.ar(DC.ar(0), length = foo) sum.poll(1, "sum") () } }}
  * 
  * ===Examples===
  * 
  * {{{
  * // clip sine wave to modulate timbre
  * play {
  *   val hi = SinOsc.ar(0.1).linExp(-1, 1, 0.01, 1.0)
  *   Clip.ar(SinOsc.ar(300), 0, hi) * 0.2 / hi
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Wrap$ Wrap]]
  * @see [[de.sciss.synth.ugen.Fold$ Fold]]
  */
object Clip extends ProductReader[Clip] {
  /** @param in               input signal to constrain
    * @param lo               lower margin of clipping
    * @param hi               upper margin of clipping
    */
  def ir(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Clip = new Clip(scalar, in, lo, hi)
  
  /** @param in               input signal to constrain
    * @param lo               lower margin of clipping
    * @param hi               upper margin of clipping
    */
  def kr(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Clip = new Clip(control, in, lo, hi)
  
  /** @param in               input signal to constrain
    * @param lo               lower margin of clipping
    * @param hi               upper margin of clipping
    */
  def ar(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Clip = new Clip(audio, in, lo, hi)
  
  def read(in: RefMapIn, arity: Int): Clip = {
    require (arity == 4)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    new Clip(_rate, _in, _lo, _hi)
  }
}

/** A UGen that constrains a signal to a given range, by limiting values outside
  * the range to the range margins. This is similar to the `clip2` binary operator
  * but permits both a lower range value `lo` and an upper range value `hi` .
  * 
  * Mathematically, this is equivalent to `in.max(lo).min(hi)`.
  * 
  * Be aware that there seems to be an initialization bug. The following crashes,
  * indicating that `Clip.ar` outputs a zero initially:
  * 
  * {{ play { val bar = Integrator.ar(DC.ar(0), coeff = 0.999) val foo =
  * Clip.ar(bar, lo = 1.0, hi = 44100.0) // .max(1.0) val sum =
  * RunningSum.ar(DC.ar(0), length = foo) sum.poll(1, "sum") () } }}
  * 
  * @param in               input signal to constrain
  * @param lo               lower margin of clipping
  * @param hi               upper margin of clipping
  * 
  * @see [[de.sciss.synth.ugen.Wrap$ Wrap]]
  * @see [[de.sciss.synth.ugen.Fold$ Fold]]
  */
final case class Clip(rate: Rate, in: GE, lo: GE = 0.0f, hi: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that produces a psychoacoustic amplitude compensation factor for a given
  * frequency.
  * 
  * Implements the formula: `(root / freq).pow(exp)`
  * 
  * Higher frequencies are normally perceived as louder, therefore `AmpComp`
  * outputs lower values for them. For example, with default parameters, the pitch
  * C4 (frequency 262 Hz) produces the base factor of 1.0, whereas a pitch one
  * octave up, C5 (or 523 Hz) produces a factor of 0.793719 (an attenuation of -2
  * dB).
  * 
  * An alternative is `AmpCompA` that better models the bell-shaped equal loudness
  * contours of the hearing system. Especially note that the output of this UGen can
  * become very high for frequencies much lower than the `root` parameter.
  * 
  * ===Examples===
  * 
  * {{{
  * // activate with mouse button
  * play {
  *   val freq = MouseX.kr(300, 15000, 1)
  *   val mod  = freq * SinOsc.ar(MouseY.kr(3, 200, 1)).mulAdd(0.5, 1)
  *   val corr = AmpComp.ar(mod, 300) * 2
  *   val amp  = Select.ar(MouseButton.kr(lag = 0), Seq(DC.ar(1), corr))
  *   SinOsc.ar(mod) * 0.1 * amp
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.AmpCompA$ AmpCompA]]
  */
object AmpComp extends ProductReader[AmpComp] {
  def ir: AmpComp = ir()
  
  /** @param freq             the frequency in Hertz for which to determine the
    *                         compensation factor
    * @param root             the base frequency corresponding to a compensation
    *                         factor of 1.0
    * @param expon            the exponent determines how steep the compensation
    *                         curve decreases for increasing frequencies. In general,
    *                         the louder a signal is played, the shallower the equal
    *                         loudness contours become.
    */
  def ir(freq: GE = 261.626f, root: GE = 261.626f, expon: GE = 0.3333f): AmpComp = 
    new AmpComp(scalar, freq, root, expon)
  
  def kr: AmpComp = kr()
  
  /** @param freq             the frequency in Hertz for which to determine the
    *                         compensation factor
    * @param root             the base frequency corresponding to a compensation
    *                         factor of 1.0
    * @param expon            the exponent determines how steep the compensation
    *                         curve decreases for increasing frequencies. In general,
    *                         the louder a signal is played, the shallower the equal
    *                         loudness contours become.
    */
  def kr(freq: GE = 261.626f, root: GE = 261.626f, expon: GE = 0.3333f): AmpComp = 
    new AmpComp(control, freq, root, expon)
  
  def ar: AmpComp = ar()
  
  /** @param freq             the frequency in Hertz for which to determine the
    *                         compensation factor
    * @param root             the base frequency corresponding to a compensation
    *                         factor of 1.0
    * @param expon            the exponent determines how steep the compensation
    *                         curve decreases for increasing frequencies. In general,
    *                         the louder a signal is played, the shallower the equal
    *                         loudness contours become.
    */
  def ar(freq: GE = 261.626f, root: GE = 261.626f, expon: GE = 0.3333f): AmpComp = 
    new AmpComp(audio, freq, root, expon)
  
  def read(in: RefMapIn, arity: Int): AmpComp = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _freq   = in.readGE()
    val _root   = in.readGE()
    val _expon  = in.readGE()
    new AmpComp(_rate, _freq, _root, _expon)
  }
}

/** A UGen that produces a psychoacoustic amplitude compensation factor for a given
  * frequency.
  * 
  * Implements the formula: `(root / freq).pow(exp)`
  * 
  * Higher frequencies are normally perceived as louder, therefore `AmpComp`
  * outputs lower values for them. For example, with default parameters, the pitch
  * C4 (frequency 262 Hz) produces the base factor of 1.0, whereas a pitch one
  * octave up, C5 (or 523 Hz) produces a factor of 0.793719 (an attenuation of -2
  * dB).
  * 
  * An alternative is `AmpCompA` that better models the bell-shaped equal loudness
  * contours of the hearing system. Especially note that the output of this UGen can
  * become very high for frequencies much lower than the `root` parameter.
  * 
  * @param freq             the frequency in Hertz for which to determine the
  *                         compensation factor
  * @param root             the base frequency corresponding to a compensation
  *                         factor of 1.0
  * @param expon            the exponent determines how steep the compensation
  *                         curve decreases for increasing frequencies. In general,
  *                         the louder a signal is played, the shallower the equal
  *                         loudness contours become.
  * 
  * @see [[de.sciss.synth.ugen.AmpCompA$ AmpCompA]]
  */
final case class AmpComp(rate: Rate, freq: GE = 261.626f, root: GE = 261.626f, expon: GE = 0.3333f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, root.expand, expon.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A UGen that produces a psychoacoustic amplitude compensation factor for a given
  * frequency. It uses the A-weighting curve that is based on the Fletcher-Munson
  * curve for rather low volume sounds (40 phon).
  * 
  * Only the `freq` parameter can be modulated, the other parameters are read at
  * initialization time only.
  * 
  * ===Examples===
  * 
  * {{{
  * // activate with mouse button
  * play {
  *   val freq = MouseX.kr(300, 15000, 1)
  *   val mod  = freq * SinOsc.ar(MouseY.kr(3, 200, 1)).mulAdd(0.5, 1)
  *   val corr = AmpCompA.ar(mod, 300) * 2
  *   val amp  = Select.ar(MouseButton.kr(lag = 0), Seq(DC.ar(1), corr))
  *   SinOsc.ar(mod) * 0.1 * amp
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.AmpComp$ AmpComp]]
  */
object AmpCompA extends ProductReader[AmpCompA] {
  def ir: AmpCompA = ir()
  
  /** @param freq             the frequency in Hertz for which to determine the
    *                         compensation factor
    * @param root             the root frequency in Hertz, relative to which the
    *                         curve is calculated. This is usually lowest expected
    *                         frequency. ''(init-time only)''
    * @param minAmp           amplitude at the minimum point of the curve. This is
    *                         the factor output when `freq` is approx. 2512 Hz.
    *                         ''(init-time only)''
    * @param rootAmp          amplitude at the root frequency of the curve. This is
    *                         the factor output when `freq == root` . ''(init-time
    *                         only)''
    */
  def ir(freq: GE = 1000.0f, root: GE = 0.0f, minAmp: GE = 0.32f, rootAmp: GE = 1.0f): AmpCompA = 
    new AmpCompA(scalar, freq, root, minAmp, rootAmp)
  
  def kr: AmpCompA = kr()
  
  /** @param freq             the frequency in Hertz for which to determine the
    *                         compensation factor
    * @param root             the root frequency in Hertz, relative to which the
    *                         curve is calculated. This is usually lowest expected
    *                         frequency. ''(init-time only)''
    * @param minAmp           amplitude at the minimum point of the curve. This is
    *                         the factor output when `freq` is approx. 2512 Hz.
    *                         ''(init-time only)''
    * @param rootAmp          amplitude at the root frequency of the curve. This is
    *                         the factor output when `freq == root` . ''(init-time
    *                         only)''
    */
  def kr(freq: GE = 1000.0f, root: GE = 0.0f, minAmp: GE = 0.32f, rootAmp: GE = 1.0f): AmpCompA = 
    new AmpCompA(control, freq, root, minAmp, rootAmp)
  
  def ar: AmpCompA = ar()
  
  /** @param freq             the frequency in Hertz for which to determine the
    *                         compensation factor
    * @param root             the root frequency in Hertz, relative to which the
    *                         curve is calculated. This is usually lowest expected
    *                         frequency. ''(init-time only)''
    * @param minAmp           amplitude at the minimum point of the curve. This is
    *                         the factor output when `freq` is approx. 2512 Hz.
    *                         ''(init-time only)''
    * @param rootAmp          amplitude at the root frequency of the curve. This is
    *                         the factor output when `freq == root` . ''(init-time
    *                         only)''
    */
  def ar(freq: GE = 1000.0f, root: GE = 0.0f, minAmp: GE = 0.32f, rootAmp: GE = 1.0f): AmpCompA = 
    new AmpCompA(audio, freq, root, minAmp, rootAmp)
  
  def read(in: RefMapIn, arity: Int): AmpCompA = {
    require (arity == 5)
    val _rate     = in.readRate()
    val _freq     = in.readGE()
    val _root     = in.readGE()
    val _minAmp   = in.readGE()
    val _rootAmp  = in.readGE()
    new AmpCompA(_rate, _freq, _root, _minAmp, _rootAmp)
  }
}

/** A UGen that produces a psychoacoustic amplitude compensation factor for a given
  * frequency. It uses the A-weighting curve that is based on the Fletcher-Munson
  * curve for rather low volume sounds (40 phon).
  * 
  * Only the `freq` parameter can be modulated, the other parameters are read at
  * initialization time only.
  * 
  * @param freq             the frequency in Hertz for which to determine the
  *                         compensation factor
  * @param root             the root frequency in Hertz, relative to which the
  *                         curve is calculated. This is usually lowest expected
  *                         frequency. ''(init-time only)''
  * @param minAmp           amplitude at the minimum point of the curve. This is
  *                         the factor output when `freq` is approx. 2512 Hz.
  *                         ''(init-time only)''
  * @param rootAmp          amplitude at the root frequency of the curve. This is
  *                         the factor output when `freq == root` . ''(init-time
  *                         only)''
  * 
  * @see [[de.sciss.synth.ugen.AmpComp$ AmpComp]]
  */
final case class AmpCompA(rate: Rate, freq: GE = 1000.0f, root: GE = 0.0f, minAmp: GE = 0.32f, rootAmp: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, root.expand, minAmp.expand, rootAmp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A UGen that tests if a signal is within a given range. If `in >= lo` and
  * `in <= hi` , outputs 1.0, otherwise outputs 0.0.
  * 
  * ===Examples===
  * 
  * {{{
  * // detect whether mouse is in specific horizontal range
  * play {
  *   val x = MouseX.kr
  *   InRange.kr(x, 0.4, 0.6) * PinkNoise.ar(0.3)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.InRect$ InRect]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  * @see [[de.sciss.synth.ugen.Schmidt$ Schmidt]]
  */
object InRange extends ProductReader[InRange] {
  /** @param in               input signal to test
    * @param lo               lower margin of test range (inclusive)
    * @param hi               upper margin of test range (inclusive)
    */
  def ir(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): InRange = new InRange(scalar, in, lo, hi)
  
  /** @param in               input signal to test
    * @param lo               lower margin of test range (inclusive)
    * @param hi               upper margin of test range (inclusive)
    */
  def kr(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): InRange = new InRange(control, in, lo, hi)
  
  /** @param in               input signal to test
    * @param lo               lower margin of test range (inclusive)
    * @param hi               upper margin of test range (inclusive)
    */
  def ar(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): InRange = new InRange(audio, in, lo, hi)
  
  def read(in: RefMapIn, arity: Int): InRange = {
    require (arity == 4)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    new InRange(_rate, _in, _lo, _hi)
  }
}

/** A UGen that tests if a signal is within a given range. If `in >= lo` and
  * `in <= hi` , outputs 1.0, otherwise outputs 0.0.
  * 
  * @param in               input signal to test
  * @param lo               lower margin of test range (inclusive)
  * @param hi               upper margin of test range (inclusive)
  * 
  * @see [[de.sciss.synth.ugen.InRect$ InRect]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  * @see [[de.sciss.synth.ugen.Schmidt$ Schmidt]]
  */
final case class InRange(rate: Rate, in: GE, lo: GE = 0.0f, hi: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that tests if two signals lie both within a given ranges. The two input
  * signals can be understood as horizontal and vertical coordinates, therefore the
  * test become one that determines whether the input is within a given "rectangle".
  * 
  * If `x >= left` and `x <= right` and `y > top` and `y <= bottom` , outputs 1.0,
  * otherwise outputs 0.0.
  * 
  * ===Examples===
  * 
  * {{{
  * // detect whether mouse is in specific horizontal and vertical range
  * play {
  *   val x  = MouseX.kr; val y = MouseY.kr(1, 0)
  *   val in = InRect.kr(x = x, y = y, left = 0.4, top = 0.2, right = 0.6, bottom = 0.4)
  *   in * PinkNoise.ar(0.3)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.InRange$ InRange]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
object InRect extends ProductReader[InRect] {
  /** @param x                "horizontal" signal to test
    * @param y                "vertical" signal to test
    * @param left             lower margin of horizontal test range (inclusive)
    * @param top              lower margin of vertical test range (inclusive)
    * @param right            upper margin of horizontal test range (inclusive)
    * @param bottom           upper margin of vertical test range (inclusive)
    */
  def ir(x: GE, y: GE, left: GE = 0.0f, top: GE = 0.0f, right: GE = 1.0f, bottom: GE = 1.0f): InRect = 
    new InRect(scalar, x, y, left, top, right, bottom)
  
  /** @param x                "horizontal" signal to test
    * @param y                "vertical" signal to test
    * @param left             lower margin of horizontal test range (inclusive)
    * @param top              lower margin of vertical test range (inclusive)
    * @param right            upper margin of horizontal test range (inclusive)
    * @param bottom           upper margin of vertical test range (inclusive)
    */
  def kr(x: GE, y: GE, left: GE = 0.0f, top: GE = 0.0f, right: GE = 1.0f, bottom: GE = 1.0f): InRect = 
    new InRect(control, x, y, left, top, right, bottom)
  
  /** @param x                "horizontal" signal to test
    * @param y                "vertical" signal to test
    * @param left             lower margin of horizontal test range (inclusive)
    * @param top              lower margin of vertical test range (inclusive)
    * @param right            upper margin of horizontal test range (inclusive)
    * @param bottom           upper margin of vertical test range (inclusive)
    */
  def ar(x: GE, y: GE, left: GE = 0.0f, top: GE = 0.0f, right: GE = 1.0f, bottom: GE = 1.0f): InRect = 
    new InRect(audio, x, y, left, top, right, bottom)
  
  def read(in: RefMapIn, arity: Int): InRect = {
    require (arity == 7)
    val _rate   = in.readRate()
    val _x      = in.readGE()
    val _y      = in.readGE()
    val _left   = in.readGE()
    val _top    = in.readGE()
    val _right  = in.readGE()
    val _bottom = in.readGE()
    new InRect(_rate, _x, _y, _left, _top, _right, _bottom)
  }
}

/** A UGen that tests if two signals lie both within a given ranges. The two input
  * signals can be understood as horizontal and vertical coordinates, therefore the
  * test become one that determines whether the input is within a given "rectangle".
  * 
  * If `x >= left` and `x <= right` and `y > top` and `y <= bottom` , outputs 1.0,
  * otherwise outputs 0.0.
  * 
  * @param x                "horizontal" signal to test
  * @param y                "vertical" signal to test
  * @param left             lower margin of horizontal test range (inclusive)
  * @param top              lower margin of vertical test range (inclusive)
  * @param right            upper margin of horizontal test range (inclusive)
  * @param bottom           upper margin of vertical test range (inclusive)
  * 
  * @see [[de.sciss.synth.ugen.InRange$ InRange]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
final case class InRect(rate: Rate, x: GE, y: GE, left: GE = 0.0f, top: GE = 0.0f, right: GE = 1.0f, bottom: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](x.expand, y.expand, left.expand, top.expand, right.expand, bottom.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen which maps a linear range to an exponential range. The equivalent
  * formula is `(dstHi / dstLo).pow((in - srcLo) / (srcHi - srcLo)) * dstLo` .
  * 
  * '''Note''': No clipping is performed. If the input signal exceeds the input
  * range, the output will also exceed its range.
  * 
  * ===Examples===
  * 
  * {{{
  * // translate linear noise into exponential frequencies
  * play {
  *   val mod = LFNoise2.ar(10)
  *   val lo  = MouseX.kr(200, 8000, 1)
  *   val hi  = MouseY.kr(200, 8000, 1)
  *   SinOsc.ar(LinExp.ar(mod, -1, 1, lo, hi)) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LinExp$ LinExp]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
object LinExp extends ProductReader[LinExp] {
  /** @param in               input signal to convert
    * @param srcLo            lower limit of input range
    * @param srcHi            upper limit of input range
    * @param dstLo            lower limit of output range
    * @param dstHi            upper limit of output range
    */
  def ir(in: GE, srcLo: GE = 0.0f, srcHi: GE = 1.0f, dstLo: GE = 1.0f, dstHi: GE = 2.0f): LinExp = 
    new LinExp(scalar, in, srcLo, srcHi, dstLo, dstHi)
  
  /** @param in               input signal to convert
    * @param srcLo            lower limit of input range
    * @param srcHi            upper limit of input range
    * @param dstLo            lower limit of output range
    * @param dstHi            upper limit of output range
    */
  def kr(in: GE, srcLo: GE = 0.0f, srcHi: GE = 1.0f, dstLo: GE = 1.0f, dstHi: GE = 2.0f): LinExp = 
    new LinExp(control, in, srcLo, srcHi, dstLo, dstHi)
  
  /** @param in               input signal to convert
    * @param srcLo            lower limit of input range
    * @param srcHi            upper limit of input range
    * @param dstLo            lower limit of output range
    * @param dstHi            upper limit of output range
    */
  def ar(in: GE, srcLo: GE = 0.0f, srcHi: GE = 1.0f, dstLo: GE = 1.0f, dstHi: GE = 2.0f): LinExp = 
    new LinExp(audio, in, srcLo, srcHi, dstLo, dstHi)
  
  def read(in: RefMapIn, arity: Int): LinExp = {
    require (arity == 6)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _srcLo  = in.readGE()
    val _srcHi  = in.readGE()
    val _dstLo  = in.readGE()
    val _dstHi  = in.readGE()
    new LinExp(_rate, _in, _srcLo, _srcHi, _dstLo, _dstHi)
  }
}

/** A UGen which maps a linear range to an exponential range. The equivalent
  * formula is `(dstHi / dstLo).pow((in - srcLo) / (srcHi - srcLo)) * dstLo` .
  * 
  * '''Note''': No clipping is performed. If the input signal exceeds the input
  * range, the output will also exceed its range.
  * 
  * @param in               input signal to convert
  * @param srcLo            lower limit of input range
  * @param srcHi            upper limit of input range
  * @param dstLo            lower limit of output range
  * @param dstHi            upper limit of output range
  * 
  * @see [[de.sciss.synth.ugen.LinExp$ LinExp]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  */
final case class LinExp(rate: MaybeRate, in: GE, srcLo: GE = 0.0f, srcHi: GE = 1.0f, dstLo: GE = 1.0f, dstHi: GE = 2.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, srcLo.expand, srcHi.expand, dstLo.expand, dstHi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** An envelope generator UGen. It uses a break point description in its `envelope`
  * input, typically coming from an `Env` object. The envelope may be re-triggered
  * using the `gate` input. Upon start and upon re-triggering, the `envelope` ,
  * `levelScale` , `levelBias` and `timeScale` parameters are polled and remain
  * constant for the duration of the envelope.
  * 
  * To construct a manual envelope without the use of the `Env` class, the format
  * should be as follows:
  * {{{
  * val env = Seq[GE](startLevel, numSegments, releaseNode, loopNode,
  * targetLevel1, duration1, curveType1, curvature1,
  * targetLevel2, duration2, curveType2, curvature2,
  * ...)
  * }}}
  * 
  * Where the curve-type is one of `Curve.step.id` , `Curve.lin.id` ,
  * `Curve.exp.id` , etc. The curvature values are only relevant for the parametric
  * curve type. The `releaseNode` and `loopNode` parameters are segment indices or
  * the special value `-99` indicating that there are no release or loop segments.
  * 
  * '''Note''': The actual minimum duration of a segment is not zero, but one
  * sample step for audio rate and one block for control rate. This may result in
  * asynchronicity when in two envelopes of different number of levels, the envelope
  * times add up to the same total duration. Similarly, when modulating times, the
  * new time is only updated at the end of the current segment; this may lead to
  * asynchronicity of two envelopes with modulated times.
  * 
  * ===Examples===
  * 
  * {{{
  * // percussive one-shot
  * play { PinkNoise.ar(EnvGen.kr(Env.perc, doneAction = freeSelf)) }
  * }}}
  * {{{
  * // fixed duration amplitude envelope
  * play {
  *   val env = Env(0, Seq(0.01 -> 1, 0.5 -> 0.5, 0.02 -> 1, 0.5 -> 0))
  *   SinOsc.ar(470) * EnvGen.kr(env, doneAction = freeSelf)
  * }
  * }}}
  * {{{
  * // amplitude and frequency modulation
  * play {
  *   val env  = Env(0, Seq(0.01 -> 1, 0.5 -> 0.5, 0.02 -> 0.8, 0.5 -> 0, 0.2 -> 1.2, 0.5 -> 0))
  *   val gate = Impulse.kr(MouseX.kr(0.2, 3), 0.5)
  *   val gen  = EnvGen.kr(env, gate)
  *   SinOsc.ar(270, SinOsc.ar(gen * 473)) * gen * 0.2
  * }
  * }}}
  * {{{
  * // Dust-triggered envelope
  * play {
  *   val c   = Curve.parametric(-4)
  *   val env = Env(0, Seq((0.05,0.5,c), (0.1,0.0,c), (0.01,1.0,c), (1.0,0.9,c), (1.5,0.0,c)))
  *   val gen = EnvGen.ar(env, Dust.ar(1))
  *   SinOsc.ar(gen * 1000 + 440) * gen * 0.1
  * }
  * }}}
  * {{{
  * // two channels
  * play {
  *   val p = Curve.parametric(-4)
  *   
  *   def mkEnv(a: Double, b: Double) = {
  *     val env = Env(0.0, Seq((0.05,a,p), (0.1,0.0,p), (0.01,1.0,p), (1.0,b,p), (1.5,0.0,p)))
  *     EnvGen.ar(env, Dust.ar(1))
  *   }
  *   
  *   val gen: GE = Seq(mkEnv(-0.2, -0.4), mkEnv(0.5, 0.9))
  *   SinOsc.ar(gen * 440 + 550) * gen * 0.1
  * }
  * }}}
  * {{{
  * // control gate and done-action
  * val x = play {
  *   var gen = EnvGen.kr(Env.adsr(), "gate".kr(0), doneAction = "done".kr(0))
  *   SinOsc.ar(440) * gen * 0.1
  * }
  * 
  * x.set("gate" -> 1)  // turn on
  * x.set("gate" -> 0)  // turn off
  * x.set("gate" -> 1)  // turn on
  * x.set("done" -> freeSelf.id, "gate" -> 0) // turn off and free
  * }}}
  * {{{
  * // fast triggering
  * play {
  *   val freq = MouseX.kr(1, 100, 1)
  *   val gate = Impulse.ar(freq)
  *   val env  = Env.perc(0.1, 0.9)
  *   val gen  = EnvGen.ar(env, gate = gate, timeScale = freq.reciprocal)
  *   SinOsc.ar(440) * gen * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Env$ Env]]
  * @see [[de.sciss.synth.ugen.IEnvGen$ IEnvGen]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  * @see [[de.sciss.synth.ugen.XLine$ XLine]]
  * @see [[de.sciss.synth.ugen.Linen$ Linen]]
  * @see [[de.sciss.synth.ugen.Decay$ Decay]]
  */
object EnvGen extends ProductReader[EnvGen] {
  /** @param envelope         the description of the envelope break-points. Typically
    *                         you pass an instance of `Env` which will then
    *                         automatically expand to the correct format.
    * @param gate             triggers the envelope and holds it open while greater
    *                         than zero. If the envelope is of fixed duration (e.g.
    *                         `Env.linen` , `Env.perc` ), the `gate` argument is used
    *                         as a simple trigger. If it contains a sustained segment
    *                         (e.g. `Env.adsr` , `Env.asr` ), the envelope is held
    *                         open until the gate becomes 0, at which point is
    *                         released. If `gate` is less than zero, a release is
    *                         enforced with duration `-1.0 - gate` .
    * @param levelScale       amplitude factor with which the nominal envelope is
    *                         multiplied.
    * @param levelBias        amplitude offset which is added to the nominal envelope.
    * @param timeScale        time scale factor with which the envelope segment
    *                         durations are multiplied.
    * @param doneAction       action to be performed when the envelope reaches its
    *                         end point.
    */
  def kr(envelope: GE, gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f, doneAction: GE = doNothing): EnvGen = 
    new EnvGen(control, envelope, gate, levelScale, levelBias, timeScale, doneAction)
  
  /** @param envelope         the description of the envelope break-points. Typically
    *                         you pass an instance of `Env` which will then
    *                         automatically expand to the correct format.
    * @param gate             triggers the envelope and holds it open while greater
    *                         than zero. If the envelope is of fixed duration (e.g.
    *                         `Env.linen` , `Env.perc` ), the `gate` argument is used
    *                         as a simple trigger. If it contains a sustained segment
    *                         (e.g. `Env.adsr` , `Env.asr` ), the envelope is held
    *                         open until the gate becomes 0, at which point is
    *                         released. If `gate` is less than zero, a release is
    *                         enforced with duration `-1.0 - gate` .
    * @param levelScale       amplitude factor with which the nominal envelope is
    *                         multiplied.
    * @param levelBias        amplitude offset which is added to the nominal envelope.
    * @param timeScale        time scale factor with which the envelope segment
    *                         durations are multiplied.
    * @param doneAction       action to be performed when the envelope reaches its
    *                         end point.
    */
  def ar(envelope: GE, gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f, doneAction: GE = doNothing): EnvGen = 
    new EnvGen(audio, envelope, gate, levelScale, levelBias, timeScale, doneAction)
  
  def read(in: RefMapIn, arity: Int): EnvGen = {
    require (arity == 7)
    val _rate       = in.readRate()
    val _envelope   = in.readGE()
    val _gate       = in.readGE()
    val _levelScale = in.readGE()
    val _levelBias  = in.readGE()
    val _timeScale  = in.readGE()
    val _doneAction = in.readGE()
    new EnvGen(_rate, _envelope, _gate, _levelScale, _levelBias, _timeScale, _doneAction)
  }
}

/** An envelope generator UGen. It uses a break point description in its `envelope`
  * input, typically coming from an `Env` object. The envelope may be re-triggered
  * using the `gate` input. Upon start and upon re-triggering, the `envelope` ,
  * `levelScale` , `levelBias` and `timeScale` parameters are polled and remain
  * constant for the duration of the envelope.
  * 
  * To construct a manual envelope without the use of the `Env` class, the format
  * should be as follows:
  * {{{
  * val env = Seq[GE](startLevel, numSegments, releaseNode, loopNode,
  * targetLevel1, duration1, curveType1, curvature1,
  * targetLevel2, duration2, curveType2, curvature2,
  * ...)
  * }}}
  * 
  * Where the curve-type is one of `Curve.step.id` , `Curve.lin.id` ,
  * `Curve.exp.id` , etc. The curvature values are only relevant for the parametric
  * curve type. The `releaseNode` and `loopNode` parameters are segment indices or
  * the special value `-99` indicating that there are no release or loop segments.
  * 
  * '''Note''': The actual minimum duration of a segment is not zero, but one
  * sample step for audio rate and one block for control rate. This may result in
  * asynchronicity when in two envelopes of different number of levels, the envelope
  * times add up to the same total duration. Similarly, when modulating times, the
  * new time is only updated at the end of the current segment; this may lead to
  * asynchronicity of two envelopes with modulated times.
  * 
  * @param envelope         the description of the envelope break-points. Typically
  *                         you pass an instance of `Env` which will then
  *                         automatically expand to the correct format.
  * @param gate             triggers the envelope and holds it open while greater
  *                         than zero. If the envelope is of fixed duration (e.g.
  *                         `Env.linen` , `Env.perc` ), the `gate` argument is used
  *                         as a simple trigger. If it contains a sustained segment
  *                         (e.g. `Env.adsr` , `Env.asr` ), the envelope is held
  *                         open until the gate becomes 0, at which point is
  *                         released. If `gate` is less than zero, a release is
  *                         enforced with duration `-1.0 - gate` .
  * @param levelScale       amplitude factor with which the nominal envelope is
  *                         multiplied.
  * @param levelBias        amplitude offset which is added to the nominal envelope.
  * @param timeScale        time scale factor with which the envelope segment
  *                         durations are multiplied.
  * @param doneAction       action to be performed when the envelope reaches its
  *                         end point.
  * 
  * @see [[de.sciss.synth.ugen.Env$ Env]]
  * @see [[de.sciss.synth.ugen.IEnvGen$ IEnvGen]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  * @see [[de.sciss.synth.ugen.XLine$ XLine]]
  * @see [[de.sciss.synth.ugen.Linen$ Linen]]
  * @see [[de.sciss.synth.ugen.Decay$ Decay]]
  */
final case class EnvGen(rate: Rate, envelope: GE, gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](gate.expand, levelScale.expand, levelBias.expand, timeScale.expand, doneAction.expand).++(envelope.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** A linear ASR-type envelope generator UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // repeated trigger
  * play {
  *   val gen = Linen.kr(Impulse.kr(2), 0.01, 0.6, 1.0)
  *   SinOsc.ar(440) * gen * 0.1
  * }
  * }}}
  * {{{
  * // play once and end the synth
  * play {
  *   val gen = Linen.kr(Impulse.kr(0), 0.01, 0.6, 1.0, doneAction = freeSelf)
  *   SinOsc.ar(440) * gen * 0.1
  * }
  * }}}
  * {{{
  * // play once and sustain
  * val x = play {
  *   val gen = Linen.kr("gate".kr(1), 0.01, 0.6, 1.0, doneAction = freeSelf)
  *   SinOsc.ar(440) * gen * 0.1
  * }
  * 
  * x.release(4)    // release envelope with given duration
  * }}}
  * {{{
  * // longer gate to sustain for a given duration
  * play {
  *   val gate = Trig.kr(1, dur = 2)
  *   val gen = Linen.kr(gate, 0.01, 0.6, 1.0, doneAction = freeSelf)
  *   SinOsc.ar(440) * gen * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  */
object Linen extends ProductReader[Linen] {
  def kr: Linen = kr()
  
  /** @param gate             triggers the envelope and holds it open while greater
    *                         than zero. A value of less than zero enforces a release
    *                         with duration `-1.0 - gate` .
    * @param attack           duration (seconds) of the attack segment
    * @param sustain          level of the sustain segment
    * @param release          duration (seconds) of the release segment
    * @param doneAction       action to be performed when the envelope reaches its
    *                         end point.
    */
  def kr(gate: GE = 1, attack: GE = 0.01f, sustain: GE = 1.0f, release: GE = 1.0f, doneAction: GE = doNothing): Linen = 
    new Linen(control, gate, attack, sustain, release, doneAction)
  
  def read(in: RefMapIn, arity: Int): Linen = {
    require (arity == 6)
    val _rate       = in.readRate()
    val _gate       = in.readGE()
    val _attack     = in.readGE()
    val _sustain    = in.readGE()
    val _release    = in.readGE()
    val _doneAction = in.readGE()
    new Linen(_rate, _gate, _attack, _sustain, _release, _doneAction)
  }
}

/** A linear ASR-type envelope generator UGen.
  * 
  * @param gate             triggers the envelope and holds it open while greater
  *                         than zero. A value of less than zero enforces a release
  *                         with duration `-1.0 - gate` .
  * @param attack           duration (seconds) of the attack segment
  * @param sustain          level of the sustain segment
  * @param release          duration (seconds) of the release segment
  * @param doneAction       action to be performed when the envelope reaches its
  *                         end point.
  * 
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  */
final case class Linen(rate: Rate, gate: GE = 1, attack: GE = 0.01f, sustain: GE = 1.0f, release: GE = 1.0f, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](gate.expand, attack.expand, sustain.expand, release.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** Envelope generator UGen with random access index pointer into the break-point
  * function.
  * 
  * '''Warning''': The envelope must be generated using `IEnv` not `Env` . `IEnv`
  * has a completely different format. Using the wrong format ( `Env` ) may crash
  * the server.
  * 
  * To construct a manual envelope without the use of the `IEnv` class, the format
  * should be as follows:
  * {{{
  * val env = Seq[GE](offset, startLevel, numSegments, totalDuration,
  * duration1, curveType1, curvature1, targetLevel1,
  * duration2, curveType2, curvature2, targetLevel2
  * ...)
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse controls index
  * play {
  *   import Curve._
  *   val env = IEnv(0, Seq(
  *     (0.10, 0.6, lin),
  *     (0.02, 0.3, exp),
  *     (0.40, 1.0, parametric(-6)),
  *     (1.10, 0.0, sine)))
  *   val dur = Mix(env.segments.map(_.dur))
  *   val gen = IEnvGen.kr(env, MouseX.kr(0, dur))
  *   SinOsc.ar(gen * 500 + 440) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.IEnv$ IEnv]]
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  */
object IEnvGen extends ProductReader[IEnvGen] {
  /** @param envelope         the description of the envelope break-points. Typically
    *                         you pass an instance of `IEnv` which will then
    *                         automatically expand to the correct format.
    * @param index            index point into the envelope, given as time in seconds
    */
  def kr(envelope: GE, index: GE): IEnvGen = new IEnvGen(control, envelope, index)
  
  /** @param envelope         the description of the envelope break-points. Typically
    *                         you pass an instance of `IEnv` which will then
    *                         automatically expand to the correct format.
    * @param index            index point into the envelope, given as time in seconds
    */
  def ar(envelope: GE, index: GE): IEnvGen = new IEnvGen(audio, envelope, index)
  
  def read(in: RefMapIn, arity: Int): IEnvGen = {
    require (arity == 3)
    val _rate     = in.readRate()
    val _envelope = in.readGE()
    val _index    = in.readGE()
    new IEnvGen(_rate, _envelope, _index)
  }
}

/** Envelope generator UGen with random access index pointer into the break-point
  * function.
  * 
  * '''Warning''': The envelope must be generated using `IEnv` not `Env` . `IEnv`
  * has a completely different format. Using the wrong format ( `Env` ) may crash
  * the server.
  * 
  * To construct a manual envelope without the use of the `IEnv` class, the format
  * should be as follows:
  * {{{
  * val env = Seq[GE](offset, startLevel, numSegments, totalDuration,
  * duration1, curveType1, curvature1, targetLevel1,
  * duration2, curveType2, curvature2, targetLevel2
  * ...)
  * }}}
  * 
  * 
  * @param envelope         the description of the envelope break-points. Typically
  *                         you pass an instance of `IEnv` which will then
  *                         automatically expand to the correct format.
  * @param index            index point into the envelope, given as time in seconds
  * 
  * @see [[de.sciss.synth.ugen.IEnv$ IEnv]]
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  */
final case class IEnvGen(rate: Rate, envelope: GE, index: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](index.expand).++(envelope.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}