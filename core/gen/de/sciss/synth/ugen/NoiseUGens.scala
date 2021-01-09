// revision: 11
package de.sciss.synth
package ugen

import UGenSource._

/** A noise generator UGens whose spectrum has equal power at all frequencies.
  * Output values range from `-1` to `+1` (before being multiplied by `mul` ). The
  * RMS is approx. -4.8 dB.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { WhiteNoise.ar(Seq(0.25, 0.25)) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  */
object WhiteNoise extends ProductReader[WhiteNoise] {
  def kr: WhiteNoise = kr()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def kr(mul: GE = 1.0f): WhiteNoise = new WhiteNoise(control, mul)
  
  def ar: WhiteNoise = ar()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def ar(mul: GE = 1.0f): WhiteNoise = new WhiteNoise(audio, mul)
  
  def read(in: RefMapIn, prefix: String, arity: Int): WhiteNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _mul  = in.readGE()
    new WhiteNoise(_rate, _mul)
  }
}

/** A noise generator UGens whose spectrum has equal power at all frequencies.
  * Output values range from `-1` to `+1` (before being multiplied by `mul` ). The
  * RMS is approx. -4.8 dB.
  * 
  * @param mul              Not actually a UGen input, this argument produces a
  *                         multiplication of the output by this factor. A
  *                         multi-channel `mul` argument will cause the generation
  *                         of multiple independent noise generators.
  * 
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  */
final case class WhiteNoise(rate: Rate, mul: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](mul.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    BinaryOpUGen.Times.make1(UGen.SingleOut(name, rate, Vector.empty, isIndividual = true), _args(0))
}

/** A noise generator UGen which results from flipping random bits in a word. The
  * resulting waveform looks like a sample-and-hold function with values between
  * `-1` and `+1` (before being multiplied by `mul` ).
  * 
  * This type of noise has a high RMS level relative to its peak to peak level.
  * With approx. -4.8 dB, the RMS is the same as white noise, but the spectrum is
  * emphasized towards lower frequencies.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { GrayNoise.ar(Seq(0.2, 0.2)) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  */
object GrayNoise extends ProductReader[GrayNoise] {
  def kr: GrayNoise = kr()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def kr(mul: GE = 1.0f): GrayNoise = new GrayNoise(control, mul)
  
  def ar: GrayNoise = ar()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def ar(mul: GE = 1.0f): GrayNoise = new GrayNoise(audio, mul)
  
  def read(in: RefMapIn, prefix: String, arity: Int): GrayNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _mul  = in.readGE()
    new GrayNoise(_rate, _mul)
  }
}

/** A noise generator UGen which results from flipping random bits in a word. The
  * resulting waveform looks like a sample-and-hold function with values between
  * `-1` and `+1` (before being multiplied by `mul` ).
  * 
  * This type of noise has a high RMS level relative to its peak to peak level.
  * With approx. -4.8 dB, the RMS is the same as white noise, but the spectrum is
  * emphasized towards lower frequencies.
  * 
  * @param mul              Not actually a UGen input, this argument produces a
  *                         multiplication of the output by this factor. A
  *                         multi-channel `mul` argument will cause the generation
  *                         of multiple independent noise generators.
  * 
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  */
final case class GrayNoise(rate: Rate, mul: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](mul.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    BinaryOpUGen.Times.make1(UGen.SingleOut(name, rate, Vector.empty, isIndividual = true), _args(0))
}

/** A noise generator UGen whose values are either `-1` or `+1` (before being
  * multiplied by `mul` ). This produces the maximum energy (an RMS of 0 dB) for the
  * least peak to peak amplitude.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { ClipNoise.ar(Seq(0.2, 0.2)) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFClipNoise$ LFClipNoise]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  */
object ClipNoise extends ProductReader[ClipNoise] {
  def kr: ClipNoise = kr()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def kr(mul: GE = 1.0f): ClipNoise = new ClipNoise(control, mul)
  
  def ar: ClipNoise = ar()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def ar(mul: GE = 1.0f): ClipNoise = new ClipNoise(audio, mul)
  
  def read(in: RefMapIn, prefix: String, arity: Int): ClipNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _mul  = in.readGE()
    new ClipNoise(_rate, _mul)
  }
}

/** A noise generator UGen whose values are either `-1` or `+1` (before being
  * multiplied by `mul` ). This produces the maximum energy (an RMS of 0 dB) for the
  * least peak to peak amplitude.
  * 
  * @param mul              Not actually a UGen input, this argument produces a
  *                         multiplication of the output by this factor. A
  *                         multi-channel `mul` argument will cause the generation
  *                         of multiple independent noise generators.
  * 
  * @see [[de.sciss.synth.ugen.LFClipNoise$ LFClipNoise]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  */
final case class ClipNoise(rate: Rate, mul: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](mul.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    BinaryOpUGen.Times.make1(UGen.SingleOut(name, rate, Vector.empty, isIndividual = true), _args(0))
}

/** A noise generator UGen whose spectrum falls off in power by 3 dB per octave.
  * This gives equal power over the span of each octave. This version gives 8
  * octaves of pink noise.
  * 
  * The values produced by this UGen were observed to lie with very high
  * probability between approx. `-0.65` and `+0.81` (before being multiplied by
  * `mul` ). The RMS is approx. -16 dB.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { PinkNoise.ar(Seq(0.2, 0.2)) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object PinkNoise extends ProductReader[PinkNoise] {
  def kr: PinkNoise = kr()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def kr(mul: GE = 1.0f): PinkNoise = new PinkNoise(control, mul)
  
  def ar: PinkNoise = ar()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def ar(mul: GE = 1.0f): PinkNoise = new PinkNoise(audio, mul)
  
  def read(in: RefMapIn, prefix: String, arity: Int): PinkNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _mul  = in.readGE()
    new PinkNoise(_rate, _mul)
  }
}

/** A noise generator UGen whose spectrum falls off in power by 3 dB per octave.
  * This gives equal power over the span of each octave. This version gives 8
  * octaves of pink noise.
  * 
  * The values produced by this UGen were observed to lie with very high
  * probability between approx. `-0.65` and `+0.81` (before being multiplied by
  * `mul` ). The RMS is approx. -16 dB.
  * 
  * @param mul              Not actually a UGen input, this argument produces a
  *                         multiplication of the output by this factor. A
  *                         multi-channel `mul` argument will cause the generation
  *                         of multiple independent noise generators.
  * 
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class PinkNoise(rate: Rate, mul: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](mul.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    BinaryOpUGen.Times.make1(UGen.SingleOut(name, rate, Vector.empty, isIndividual = true), _args(0))
}

/** A noise generator UGen whose spectrum falls off in power by 6 dB per octave.
  * The values produced by this UGen lie between `-1` and `+1` , the RMS is approx.
  * -4.8 dB (the same as white noise).
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { BrownNoise.ar(Seq(0.2, 0.2)) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object BrownNoise extends ProductReader[BrownNoise] {
  def kr: BrownNoise = kr()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def kr(mul: GE = 1.0f): BrownNoise = new BrownNoise(control, mul)
  
  def ar: BrownNoise = ar()
  
  /** @param mul              Not actually a UGen input, this argument produces a
    *                         multiplication of the output by this factor. A
    *                         multi-channel `mul` argument will cause the generation
    *                         of multiple independent noise generators.
    */
  def ar(mul: GE = 1.0f): BrownNoise = new BrownNoise(audio, mul)
  
  def read(in: RefMapIn, prefix: String, arity: Int): BrownNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _mul  = in.readGE()
    new BrownNoise(_rate, _mul)
  }
}

/** A noise generator UGen whose spectrum falls off in power by 6 dB per octave.
  * The values produced by this UGen lie between `-1` and `+1` , the RMS is approx.
  * -4.8 dB (the same as white noise).
  * 
  * @param mul              Not actually a UGen input, this argument produces a
  *                         multiplication of the output by this factor. A
  *                         multi-channel `mul` argument will cause the generation
  *                         of multiple independent noise generators.
  * 
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.PinkNoise$ PinkNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class BrownNoise(rate: Rate, mul: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](mul.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    BinaryOpUGen.Times.make1(UGen.SingleOut(name, rate, Vector.empty, isIndividual = true), _args(0))
}

/** A UGen generating random impulses with values ranging from `0` to `+1` . The
  * pulse duration is one sample for audio-rate and one block for control-rate
  * operation.
  * 
  * The approximate RMS energy is `(density/sr).log2 * 3 - 4.8` where `sr` is the
  * sample-rate. For example, at 44.1 kHz, a density of 1000 Hz yields an RMS of
  * approx. -21.2 dB.
  * 
  * ===Examples===
  * 
  * {{{
  * // decreasing density
  * play { Dust.ar(XLine.kr(20000, 2, Seq(20, 20))) * 0.5 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Dust2$ Dust2]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.CoinGate$ CoinGate]]
  */
object Dust extends ProductReader[Dust] {
  def kr: Dust = kr()
  
  /** @param density          the average number of impulses per second
    */
  def kr(density: GE = 1.0f): Dust = new Dust(control, density)
  
  def ar: Dust = ar()
  
  /** @param density          the average number of impulses per second
    */
  def ar(density: GE = 1.0f): Dust = new Dust(audio, density)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Dust = {
    require (arity == 2)
    val _rate     = in.readRate()
    val _density  = in.readGE()
    new Dust(_rate, _density)
  }
}

/** A UGen generating random impulses with values ranging from `0` to `+1` . The
  * pulse duration is one sample for audio-rate and one block for control-rate
  * operation.
  * 
  * The approximate RMS energy is `(density/sr).log2 * 3 - 4.8` where `sr` is the
  * sample-rate. For example, at 44.1 kHz, a density of 1000 Hz yields an RMS of
  * approx. -21.2 dB.
  * 
  * @param density          the average number of impulses per second
  * 
  * @see [[de.sciss.synth.ugen.Dust2$ Dust2]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  * @see [[de.sciss.synth.ugen.CoinGate$ CoinGate]]
  */
final case class Dust(rate: Rate, density: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](density.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen generating random impulses with values ranging from `-1` to `+1` . The
  * pulse duration is one sample for audio-rate and one block for control-rate
  * operation.
  * 
  * ===Examples===
  * 
  * {{{
  * // decreasing density
  * play { Dust2.ar(XLine.kr(20000, 2, Seq(20, 20))) * 0.5 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.CoinGate$ CoinGate]]
  */
object Dust2 extends ProductReader[Dust2] {
  def kr: Dust2 = kr()
  
  /** @param density          the average number of impulses per second
    */
  def kr(density: GE = 1.0f): Dust2 = new Dust2(control, density)
  
  def ar: Dust2 = ar()
  
  /** @param density          the average number of impulses per second
    */
  def ar(density: GE = 1.0f): Dust2 = new Dust2(audio, density)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Dust2 = {
    require (arity == 2)
    val _rate     = in.readRate()
    val _density  = in.readGE()
    new Dust2(_rate, _density)
  }
}

/** A UGen generating random impulses with values ranging from `-1` to `+1` . The
  * pulse duration is one sample for audio-rate and one block for control-rate
  * operation.
  * 
  * @param density          the average number of impulses per second
  * 
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.CoinGate$ CoinGate]]
  */
final case class Dust2(rate: Rate, density: GE = 1.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](density.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A noise generator UGen based on a chaotic function. Output values lie between
  * zero and one. Although this is a deterministic process, it is randomly seeded.
  * 
  * ===Examples===
  * 
  * {{{
  * // increasing parameter
  * play {
  *   val chaos = Line.kr(1.0, 2.01, 15)
  *   chaos.poll(2, "chaos")
  *   Crackle.ar(Seq(chaos, chaos)) * 0.5
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  * @see [[de.sciss.synth.ugen.Logistic$ Logistic]]
  */
object Crackle extends ProductReader[Crackle] {
  def kr: Crackle = kr()
  
  /** @param chaos            A parameter of the chaotic function with useful values
    *                         from just below 1.0 to just above 2.0. Towards 2.0 the
    *                         sound crackles. Values greater than 2.01 are not safe,
    *                         as the UGen can switch to outputting NaNs. A early more
    *                         crackling sound appears with a value of `1.33` .
    */
  def kr(chaos: GE = 1.5f): Crackle = new Crackle(control, chaos)
  
  def ar: Crackle = ar()
  
  /** @param chaos            A parameter of the chaotic function with useful values
    *                         from just below 1.0 to just above 2.0. Towards 2.0 the
    *                         sound crackles. Values greater than 2.01 are not safe,
    *                         as the UGen can switch to outputting NaNs. A early more
    *                         crackling sound appears with a value of `1.33` .
    */
  def ar(chaos: GE = 1.5f): Crackle = new Crackle(audio, chaos)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Crackle = {
    require (arity == 2)
    val _rate   = in.readRate()
    val _chaos  = in.readGE()
    new Crackle(_rate, _chaos)
  }
}

/** A noise generator UGen based on a chaotic function. Output values lie between
  * zero and one. Although this is a deterministic process, it is randomly seeded.
  * 
  * @param chaos            A parameter of the chaotic function with useful values
  *                         from just below 1.0 to just above 2.0. Towards 2.0 the
  *                         sound crackles. Values greater than 2.01 are not safe,
  *                         as the UGen can switch to outputting NaNs. A early more
  *                         crackling sound appears with a value of `1.33` .
  * 
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  * @see [[de.sciss.synth.ugen.Logistic$ Logistic]]
  */
final case class Crackle(rate: Rate, chaos: GE = 1.5f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chaos.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A noise generator UGen based on the logistic map. Its formula is
  * {{{
  * y[n+1] = chaos * y[n] * (1.0 - y[n])
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // increasing parameter
  * play { Logistic.ar(Line.kr(3.55, 4.0, 15), 1000) * 0.5 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Crackle$ Crackle]]
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  */
object Logistic extends ProductReader[Logistic] {
  def kr: Logistic = kr()
  
  /** @param chaos            a parameter of the chaotic function with useful values
    *                         from 0.0 to 4.0. Chaotic behavior occurs from 3.57 up.
    *                         Using values outside this range can make the UGen blow
    *                         up, resulting in NaNs.
    * @param freq             Frequency of calculation in Hertz. The value is limited
    *                         by the sampling rate.
    * @param init             Initial value of the recursive function
    */
  def kr(chaos: GE = 3.0f, freq: GE = 1000.0f, init: GE = 0.5f): Logistic = 
    new Logistic(control, chaos, freq, init)
  
  def ar: Logistic = ar()
  
  /** @param chaos            a parameter of the chaotic function with useful values
    *                         from 0.0 to 4.0. Chaotic behavior occurs from 3.57 up.
    *                         Using values outside this range can make the UGen blow
    *                         up, resulting in NaNs.
    * @param freq             Frequency of calculation in Hertz. The value is limited
    *                         by the sampling rate.
    * @param init             Initial value of the recursive function
    */
  def ar(chaos: GE = 3.0f, freq: GE = 1000.0f, init: GE = 0.5f): Logistic = 
    new Logistic(audio, chaos, freq, init)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Logistic = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _chaos  = in.readGE()
    val _freq   = in.readGE()
    val _init   = in.readGE()
    new Logistic(_rate, _chaos, _freq, _init)
  }
}

/** A noise generator UGen based on the logistic map. Its formula is
  * {{{
  * y[n+1] = chaos * y[n] * (1.0 - y[n])
  * }}}
  * 
  * 
  * @param chaos            a parameter of the chaotic function with useful values
  *                         from 0.0 to 4.0. Chaotic behavior occurs from 3.57 up.
  *                         Using values outside this range can make the UGen blow
  *                         up, resulting in NaNs.
  * @param freq             Frequency of calculation in Hertz. The value is limited
  *                         by the sampling rate.
  * @param init             Initial value of the recursive function
  * 
  * @see [[de.sciss.synth.ugen.Crackle$ Crackle]]
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  */
final case class Logistic(rate: Rate, chaos: GE = 3.0f, freq: GE = 1000.0f, init: GE = 0.5f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chaos.expand, freq.expand, init.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that returns a unique output value from -1 to +1 for each input value
  * according to a hash function. The same input value will always produce the same
  * output value. The input values can have any range.
  * 
  * ===Examples===
  * 
  * {{{
  * // scramble mouse motion
  * play {
  *   SinOsc.ar(
  *     Hasher.kr(MouseX.kr(0,10)).mulAdd(300, 500)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // distort pulse wave
  * play {
  *   val lag  = MouseY.kr(0.001, 0.1, 1)
  *   val freq = MouseX.kr(1, 500, 1)
  *   Hasher.ar(Lag.ar(LFPulse.ar(freq), lag)) * 0.2
  * }
  * }}}
  */
object Hasher extends ProductReader[Hasher] {
  /** @param in               input to calculate the hash function for
    */
  def kr(in: GE): Hasher = new Hasher(control, in)
  
  /** @param in               input to calculate the hash function for
    */
  def ar(in: GE): Hasher = new Hasher(audio, in)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Hasher = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new Hasher(_rate, _in)
  }
}

/** A UGen that returns a unique output value from -1 to +1 for each input value
  * according to a hash function. The same input value will always produce the same
  * output value. The input values can have any range.
  * 
  * @param in               input to calculate the hash function for
  */
final case class Hasher(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that masks off bits in the mantissa of the floating point sample value.
  * This introduces a quantization noise, but is less severe than linearly
  * quantizing the signal.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-x controls resolution
  * play {
  *   val bits = MouseX.kr(0, 12)
  *   MantissaMask.ar(SinOsc.ar(SinOsc.kr(0.2).mulAdd(400, 500)) * 0.4, bits)
  * }
  * }}}
  */
object MantissaMask extends ProductReader[MantissaMask] {
  /** @param in               input signal to quantize
    * @param bits             The number of mantissa bits to preserve, from 0 to 23.
    */
  def kr(in: GE, bits: GE = 3): MantissaMask = new MantissaMask(control, in, bits)
  
  /** @param in               input signal to quantize
    * @param bits             The number of mantissa bits to preserve, from 0 to 23.
    */
  def ar(in: GE, bits: GE = 3): MantissaMask = new MantissaMask(audio, in, bits)
  
  def read(in: RefMapIn, prefix: String, arity: Int): MantissaMask = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _bits = in.readGE()
    new MantissaMask(_rate, _in, _bits)
  }
}

/** A UGen that masks off bits in the mantissa of the floating point sample value.
  * This introduces a quantization noise, but is less severe than linearly
  * quantizing the signal.
  * 
  * @param in               input signal to quantize
  * @param bits             The number of mantissa bits to preserve, from 0 to 23.
  */
final case class MantissaMask(rate: MaybeRate, in: GE, bits: GE = 3) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, bits.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that randomly generates the values -1 or +1 at a rate given by the
  * nearest integer division of the sample rate by the frequency argument. The
  * difference to `LFClipNoise` is that this UGen quantizes time to the nearest
  * integer division of the sample-rate, and the frequency input is only polled at
  * the moment a new output value is scheduled.
  * 
  * ===Examples===
  * 
  * {{{
  * // generator
  * play { LFClipNoise.ar(500) * 0.2 }
  * }}}
  * {{{
  * // random panning
  * play {
  *   val pos = LFClipNoise.ar(4)
  *   Pan2.ar(PinkNoise.ar, pos)
  * }
  * }}}
  * {{{
  * // modulate frequency
  * play { LFClipNoise.ar(XLine.kr(100, 10000, 20)) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFDClipNoise$ LFDClipNoise]]
  */
object LFClipNoise extends ProductReader[LFClipNoise] {
  def kr: LFClipNoise = kr()
  
  /** @param freq             rate at which to generate random values.
    */
  def kr(freq: GE = 500.0f): LFClipNoise = new LFClipNoise(control, freq)
  
  def ar: LFClipNoise = ar()
  
  /** @param freq             rate at which to generate random values.
    */
  def ar(freq: GE = 500.0f): LFClipNoise = new LFClipNoise(audio, freq)
  
  def read(in: RefMapIn, prefix: String, arity: Int): LFClipNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFClipNoise(_rate, _freq)
  }
}

/** A UGen that randomly generates the values -1 or +1 at a rate given by the
  * nearest integer division of the sample rate by the frequency argument. The
  * difference to `LFClipNoise` is that this UGen quantizes time to the nearest
  * integer division of the sample-rate, and the frequency input is only polled at
  * the moment a new output value is scheduled.
  * 
  * @param freq             rate at which to generate random values.
  * 
  * @see [[de.sciss.synth.ugen.ClipNoise$ ClipNoise]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFDClipNoise$ LFDClipNoise]]
  */
final case class LFClipNoise(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A step noise UGen. It generates abruptly changing random values between `-1`
  * and `+1` at a rate given by the `freq` argument.
  * 
  * The frequency is quantized to the nearest integer division of the sample rate,
  * and changes in frequency are only picked up at the next trigger. In contrast,
  * variant `LFDNoise0` has precise frequency and reacts to frequency changes
  * instantly.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { LFNoise0.ar(1000) * 0.25 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFDNoise0$ LFDNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  * @see [[de.sciss.synth.ugen.LFNoise2$ LFNoise2]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object LFNoise0 extends ProductReader[LFNoise0] {
  def kr: LFNoise0 = kr()
  
  /** @param freq             rate at which to generate random values.
    */
  def kr(freq: GE = 500.0f): LFNoise0 = new LFNoise0(control, freq)
  
  def ar: LFNoise0 = ar()
  
  /** @param freq             rate at which to generate random values.
    */
  def ar(freq: GE = 500.0f): LFNoise0 = new LFNoise0(audio, freq)
  
  def read(in: RefMapIn, prefix: String, arity: Int): LFNoise0 = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFNoise0(_rate, _freq)
  }
}

/** A step noise UGen. It generates abruptly changing random values between `-1`
  * and `+1` at a rate given by the `freq` argument.
  * 
  * The frequency is quantized to the nearest integer division of the sample rate,
  * and changes in frequency are only picked up at the next trigger. In contrast,
  * variant `LFDNoise0` has precise frequency and reacts to frequency changes
  * instantly.
  * 
  * @param freq             rate at which to generate random values.
  * 
  * @see [[de.sciss.synth.ugen.LFDNoise0$ LFDNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  * @see [[de.sciss.synth.ugen.LFNoise2$ LFNoise2]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.Dust$ Dust]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class LFNoise0(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A ramp noise UGen. It generates line segments whose start and end points are
  * chosen randomly between `-1` and `+1` . New breakpoints are generated at a
  * specified frequency.
  * 
  * The frequency is quantized to the nearest integer division of the sample rate,
  * and changes in frequency are only picked up at the next trigger. In contrast,
  * variant `LFDNoise1` has precise frequency and reacts to frequency changes
  * instantly.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { LFNoise1.ar(1000) * 0.25 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFDNoise1$ LFDNoise1]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise2$ LFNoise2]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
object LFNoise1 extends ProductReader[LFNoise1] {
  def kr: LFNoise1 = kr()
  
  /** @param freq             rate at which to generate new breakpoints.
    */
  def kr(freq: GE = 500.0f): LFNoise1 = new LFNoise1(control, freq)
  
  def ar: LFNoise1 = ar()
  
  /** @param freq             rate at which to generate new breakpoints.
    */
  def ar(freq: GE = 500.0f): LFNoise1 = new LFNoise1(audio, freq)
  
  def read(in: RefMapIn, prefix: String, arity: Int): LFNoise1 = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFNoise1(_rate, _freq)
  }
}

/** A ramp noise UGen. It generates line segments whose start and end points are
  * chosen randomly between `-1` and `+1` . New breakpoints are generated at a
  * specified frequency.
  * 
  * The frequency is quantized to the nearest integer division of the sample rate,
  * and changes in frequency are only picked up at the next trigger. In contrast,
  * variant `LFDNoise1` has precise frequency and reacts to frequency changes
  * instantly.
  * 
  * @param freq             rate at which to generate new breakpoints.
  * 
  * @see [[de.sciss.synth.ugen.LFDNoise1$ LFDNoise1]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise2$ LFNoise2]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
final case class LFNoise1(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A quadratically interpolating noise UGen. This interpolation happens between
  * breakpoints chosen randomly between `-1` and `+1` at a specified frequency.
  * 
  * The frequency is quantized to the nearest integer division of the sample rate,
  * and changes in frequency are only picked up at the next trigger. In contrast,
  * variant `LFDNoise3` has precise frequency and reacts to frequency changes
  * instantly.
  * 
  * '''Note:''' Due to the interpolation, the output values can occasionally extend
  * beyond the normal range of -1 to +1, if the frequency varies in certain ways.
  * 
  * ===Examples===
  * 
  * {{{
  * // plain noise
  * play { LFNoise2.ar(1000) * 0.25 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFDNoise3$ LFDNoise3]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  */
object LFNoise2 extends ProductReader[LFNoise2] {
  def kr: LFNoise2 = kr()
  
  /** @param freq             rate at which to generate new breakpoints.
    */
  def kr(freq: GE = 500.0f): LFNoise2 = new LFNoise2(control, freq)
  
  def ar: LFNoise2 = ar()
  
  /** @param freq             rate at which to generate new breakpoints.
    */
  def ar(freq: GE = 500.0f): LFNoise2 = new LFNoise2(audio, freq)
  
  def read(in: RefMapIn, prefix: String, arity: Int): LFNoise2 = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFNoise2(_rate, _freq)
  }
}

/** A quadratically interpolating noise UGen. This interpolation happens between
  * breakpoints chosen randomly between `-1` and `+1` at a specified frequency.
  * 
  * The frequency is quantized to the nearest integer division of the sample rate,
  * and changes in frequency are only picked up at the next trigger. In contrast,
  * variant `LFDNoise3` has precise frequency and reacts to frequency changes
  * instantly.
  * 
  * '''Note:''' Due to the interpolation, the output values can occasionally extend
  * beyond the normal range of -1 to +1, if the frequency varies in certain ways.
  * 
  * @param freq             rate at which to generate new breakpoints.
  * 
  * @see [[de.sciss.synth.ugen.LFDNoise3$ LFDNoise3]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  */
final case class LFNoise2(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A scalar UGen that generates a single random decimal value, using a uniform
  * distribution from `lo` to `hi` .
  * 
  * ===Examples===
  * 
  * {{{
  * // random sine frequencies
  * play {
  *   val m = Mix.fill(10)(FSinOsc.ar(Rand(200.0, 800.0)))
  *   m * Line.kr(0.025, 0, 4, doneAction = freeSelf)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object Rand extends ProductReader[Rand] {
  def read(in: RefMapIn, prefix: String, arity: Int): Rand = {
    require (arity == 2)
    val _lo = in.readGE()
    val _hi = in.readGE()
    new Rand(_lo, _hi)
  }
}

/** A scalar UGen that generates a single random decimal value, using a uniform
  * distribution from `lo` to `hi` .
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class Rand(lo: GE = 0.0f, hi: GE = 1.0f)
  extends UGenSource.SingleOut with ScalarRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args, isIndividual = true)
}

/** A scalar UGen that generates a single random integer value, using a uniform
  * distribution from `lo` to `hi` .
  * 
  * ===Examples===
  * 
  * {{{
  * // random sine frequencies
  * play {
  *   val m = Mix.fill(10)(FSinOsc.ar(IRand(40, 100).midiCps))
  *   m * Line.kr(0.025, 0, 4, doneAction = freeSelf)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.TIRand$ TIRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object IRand extends ProductReader[IRand] {
  def read(in: RefMapIn, prefix: String, arity: Int): IRand = {
    require (arity == 2)
    val _lo = in.readGE()
    val _hi = in.readGE()
    new IRand(_lo, _hi)
  }
}

/** A scalar UGen that generates a single random integer value, using a uniform
  * distribution from `lo` to `hi` .
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range (inclusive)
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.TIRand$ TIRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class IRand(lo: GE = 0, hi: GE = 127)
  extends UGenSource.SingleOut with ScalarRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args, isIndividual = true)
}

/** A UGen that generates a new random decimal value each time it is triggered,
  * using a uniform distribution from `lo` to `hi` .
  * 
  * '''Note''': Audio-rate inputs for `lo` and `hi` are currently broken in
  * SuperCollider, and will therefore be converted to control-rate inputs.
  * 
  * ===Examples===
  * 
  * {{{
  * // random sine frequencies, triggered by mouse button
  * play {
  *   val tr = MouseButton.kr(lag = 0)
  *   val m  = Mix.fill(10)(SinOsc.ar(TRand.kr(200.0, 800.0, tr)))
  *   m * Linen.kr(tr, sustain = 0.025, release = 2)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object TRand extends ProductReader[TRand] {
  /** @param lo               lower limit of the output range
    * @param hi               upper limit of the output range
    * @param trig             signal to trigger new random number
    */
  def kr(lo: GE = 0.0f, hi: GE = 1.0f, trig: GE): TRand = new TRand(control, lo, hi, trig)
  
  /** @param lo               lower limit of the output range
    * @param hi               upper limit of the output range
    * @param trig             signal to trigger new random number
    */
  def ar(lo: GE = 0.0f, hi: GE = 1.0f, trig: GE): TRand = new TRand(audio, lo, hi, trig)
  
  def read(in: RefMapIn, prefix: String, arity: Int): TRand = {
    require (arity == 4)
    val _rate = in.readRate()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    val _trig = in.readGE()
    new TRand(_rate, _lo, _hi, _trig)
  }
}

/** A UGen that generates a new random decimal value each time it is triggered,
  * using a uniform distribution from `lo` to `hi` .
  * 
  * '''Note''': Audio-rate inputs for `lo` and `hi` are currently broken in
  * SuperCollider, and will therefore be converted to control-rate inputs.
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range
  * @param trig             signal to trigger new random number
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class TRand(rate: Rate, lo: GE = 0.0f, hi: GE = 1.0f, trig: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, control) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, control) else _args1
    UGen.SingleOut(name, rate, _args2, isIndividual = true)
  }
}

/** A UGen that generates a new random decimal value each time it is triggered,
  * using an exponential distribution from `lo` to `hi` . Values `lo` and `hi` must
  * both have the same sign and be non-zero.
  * 
  * '''Note''': Audio-rate inputs for `lo` and `hi` are currently broken in
  * SuperCollider, and will therefore be converted to control-rate inputs.
  * 
  * ===Examples===
  * 
  * {{{
  * // random sine frequencies, triggered by mouse button
  * play {
  *   val tr = MouseButton.kr(lag = 0)
  *   val m  = Mix.fill(10)(SinOsc.ar(TExpRand.kr(200.0, 800.0, tr)))
  *   m * Linen.kr(tr, sustain = 0.025, release = 2)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object TExpRand extends ProductReader[TExpRand] {
  /** @param lo               lower limit of the output range
    * @param hi               upper limit of the output range
    * @param trig             signal to trigger new random number
    */
  def kr(lo: GE = 0.01f, hi: GE = 1.0f, trig: GE): TExpRand = new TExpRand(control, lo, hi, trig)
  
  /** @param lo               lower limit of the output range
    * @param hi               upper limit of the output range
    * @param trig             signal to trigger new random number
    */
  def ar(lo: GE = 0.01f, hi: GE = 1.0f, trig: GE): TExpRand = new TExpRand(audio, lo, hi, trig)
  
  def read(in: RefMapIn, prefix: String, arity: Int): TExpRand = {
    require (arity == 4)
    val _rate = in.readRate()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    val _trig = in.readGE()
    new TExpRand(_rate, _lo, _hi, _trig)
  }
}

/** A UGen that generates a new random decimal value each time it is triggered,
  * using an exponential distribution from `lo` to `hi` . Values `lo` and `hi` must
  * both have the same sign and be non-zero.
  * 
  * '''Note''': Audio-rate inputs for `lo` and `hi` are currently broken in
  * SuperCollider, and will therefore be converted to control-rate inputs.
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range
  * @param trig             signal to trigger new random number
  * 
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class TExpRand(rate: Rate, lo: GE = 0.01f, hi: GE = 1.0f, trig: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, control) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, control) else _args1
    UGen.SingleOut(name, rate, _args2, isIndividual = true)
  }
}

/** A UGen that outputs integer random numbers when triggered. The values have a
  * uniform distribution from `lo` to `hi` (inclusive).
  * 
  * '''Note''': Audio-rate inputs for `lo` and `hi` are currently broken in
  * SuperCollider, and will therefore be converted to control-rate inputs.
  * 
  * ===Examples===
  * 
  * {{{
  * // random sine frequencies, triggered by mouse button
  * play {
  *   val tr = MouseButton.kr(lag = 0)
  *   val m  = Mix.fill(10)(SinOsc.ar(TIRand.kr(40, 100, tr).midiCps))
  *   m * Linen.kr(tr, sustain = 0.025, release = 2)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  */
object TIRand extends ProductReader[TIRand] {
  /** @param lo               lower limit of the output range
    * @param hi               upper limit of the output range (inclusive)
    * @param trig             signal to trigger new random number
    */
  def kr(lo: GE = 0, hi: GE = 127, trig: GE): TIRand = new TIRand(control, lo, hi, trig)
  
  /** @param lo               lower limit of the output range
    * @param hi               upper limit of the output range (inclusive)
    * @param trig             signal to trigger new random number
    */
  def ar(lo: GE = 0, hi: GE = 127, trig: GE): TIRand = new TIRand(audio, lo, hi, trig)
  
  def read(in: RefMapIn, prefix: String, arity: Int): TIRand = {
    require (arity == 4)
    val _rate = in.readRate()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    val _trig = in.readGE()
    new TIRand(_rate, _lo, _hi, _trig)
  }
}

/** A UGen that outputs integer random numbers when triggered. The values have a
  * uniform distribution from `lo` to `hi` (inclusive).
  * 
  * '''Note''': Audio-rate inputs for `lo` and `hi` are currently broken in
  * SuperCollider, and will therefore be converted to control-rate inputs.
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range (inclusive)
  * @param trig             signal to trigger new random number
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  */
final case class TIRand(rate: Rate, lo: GE = 0, hi: GE = 127, trig: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, control) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, control) else _args1
    UGen.SingleOut(name, rate, _args2, isIndividual = true)
  }
}

/** A scalar UGen that generates a single random decimal value, averaging a given
  * number of samples from a uniform distribution between `lo` and `hi` .
  * 
  * ===Examples===
  * 
  * {{{
  * // three clusters with different distributions
  * play {
  *   val z = 1 to 3 map { n =>
  *     Mix.fill(10)(FSinOsc.ar(NRand(200, 800, n))) * SinOsc.ar(0.4, n).max(0)
  *   }
  *   SplayAz.ar(2, z) * 0.025
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object NRand extends ProductReader[NRand] {
  def read(in: RefMapIn, prefix: String, arity: Int): NRand = {
    require (arity == 3)
    val _lo = in.readGE()
    val _hi = in.readGE()
    val _n  = in.readGE()
    new NRand(_lo, _hi, _n)
  }
}

/** A scalar UGen that generates a single random decimal value, averaging a given
  * number of samples from a uniform distribution between `lo` and `hi` .
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range
  * @param n                the number of random numbers to average. For `n = 1` ,
  *                         the result is identical to `Rand` , for `n = 2` , the
  *                         distribution is triangular, and for larger values the
  *                         distribution converges towards a Gaussian.
  *                         '''Warning''': The value should be not be less than one.
  * 
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class NRand(lo: GE = 0.0f, hi: GE = 1.0f, n: GE = 2)
  extends UGenSource.SingleOut with ScalarRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, n.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args, isIndividual = true)
}

/** A scalar UGen that generates a single random decimal value between `lo` and
  * `hi` with a selectable skew towards either end.
  * 
  * The `minMax <= 0` behaves rather odd: If `minMax` is less than 1, the
  * distribution is skewed towards `lo` (with `lo = 0` and `hi = 1` , the mean is
  * approx. 0.33). If `minMax` is greater than or equal to 1, the distribution is
  * skewed towards `hi` (with `lo = 0` and `hi = 1` , the mean is approx 0.66).
  * 
  * ===Examples===
  * 
  * {{{
  * // two clusters with opposite skew
  * play {
  *   val z: GE = 0 to 1 map { n =>
  *     Mix.fill(10)(FSinOsc.ar(LinRand(200, 800, n))) * SinOsc.ar(0.4, n * math.Pi).max(0)
  *   }
  *   z * 0.025
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.NRand$ NRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object LinRand extends ProductReader[LinRand] {
  def read(in: RefMapIn, prefix: String, arity: Int): LinRand = {
    require (arity == 3)
    val _lo     = in.readGE()
    val _hi     = in.readGE()
    val _minMax = in.readGE()
    new LinRand(_lo, _hi, _minMax)
  }
}

/** A scalar UGen that generates a single random decimal value between `lo` and
  * `hi` with a selectable skew towards either end.
  * 
  * The `minMax <= 0` behaves rather odd: If `minMax` is less than 1, the
  * distribution is skewed towards `lo` (with `lo = 0` and `hi = 1` , the mean is
  * approx. 0.33). If `minMax` is greater than or equal to 1, the distribution is
  * skewed towards `hi` (with `lo = 0` and `hi = 1` , the mean is approx 0.66).
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range
  * @param minMax           if `0` , the output is skewed towards `lo` , if `1` ,
  *                         the output is skewed towards `hi` .
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.ExpRand$ ExpRand]]
  * @see [[de.sciss.synth.ugen.NRand$ NRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class LinRand(lo: GE = 0.0f, hi: GE = 1.0f, minMax: GE = 0)
  extends UGenSource.SingleOut with ScalarRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, minMax.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args, isIndividual = true)
}

/** A scalar UGen that generates a single random decimal value, using an
  * exponential distribution from `lo` to `hi` . Values `lo` and `hi` must both have
  * the same sign and be non-zero.
  * 
  * ===Examples===
  * 
  * {{{
  * // random sine frequencies
  * play {
  *   val m = Mix.fill(10)(FSinOsc.ar(ExpRand(200.0, 800.0)))
  *   m * Line.kr(0.025, 0, 4, doneAction = freeSelf)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.TExpRand$ TExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
object ExpRand extends ProductReader[ExpRand] {
  def read(in: RefMapIn, prefix: String, arity: Int): ExpRand = {
    require (arity == 2)
    val _lo = in.readGE()
    val _hi = in.readGE()
    new ExpRand(_lo, _hi)
  }
}

/** A scalar UGen that generates a single random decimal value, using an
  * exponential distribution from `lo` to `hi` . Values `lo` and `hi` must both have
  * the same sign and be non-zero.
  * 
  * @param lo               lower limit of the output range
  * @param hi               upper limit of the output range
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.TExpRand$ TExpRand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  */
final case class ExpRand(lo: GE = 0.01f, hi: GE = 1.0f)
  extends UGenSource.SingleOut with ScalarRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, scalar, _args, isIndividual = true)
}

/** A UGen that randomly filters an input trigger signal. When a trigger arrives,
  * it may pass with a probability given by the `prob` argument.
  * 
  * ===Examples===
  * 
  * {{{
  * // filter dust, probability controlled by mouse
  * play {
  *   val p = MouseX.kr
  *   CoinGate.ar(Dust.ar(400), p)
  * }
  * }}}
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.Gate$ Gate]]
  * @see [[de.sciss.synth.ugen.PulseDivider$ PulseDivider]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  */
object CoinGate extends ProductReader[CoinGate] {
  /** @param in               the input triggers to filter
    * @param prob             the probability between zero (no trigger passed) and 1
    *                         (all triggers passed)
    */
  def kr(in: GE, prob: GE = 0.5f): CoinGate = new CoinGate(control, in, prob)
  
  /** @param in               the input triggers to filter
    * @param prob             the probability between zero (no trigger passed) and 1
    *                         (all triggers passed)
    */
  def ar(in: GE, prob: GE = 0.5f): CoinGate = new CoinGate(audio, in, prob)
  
  def read(in: RefMapIn, prefix: String, arity: Int): CoinGate = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _prob = in.readGE()
    new CoinGate(_rate, _in, _prob)
  }
}

/** A UGen that randomly filters an input trigger signal. When a trigger arrives,
  * it may pass with a probability given by the `prob` argument.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param in               the input triggers to filter
  * @param prob             the probability between zero (no trigger passed) and 1
  *                         (all triggers passed)
  * 
  * @see [[de.sciss.synth.ugen.Gate$ Gate]]
  * @see [[de.sciss.synth.ugen.PulseDivider$ PulseDivider]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  */
final case class CoinGate(rate: MaybeRate, in: GE, prob: GE = 0.5f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](prob.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(1).rate)
    val _args1 = matchRateT(_args, 1, _rate)
    UGen.SingleOut(name, _rate, _args1, isIndividual = true)
  }
}

/** A UGen that resets the seed of the synth's random number generator upon
  * receiving a trigger. All synths that use the same random number generator
  * reproduce the same sequence of numbers again. The generator can be set using the
  * `RandID` UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // reset seed via mouse button
  * play {
  *   val freq = TIRand.kr(40, 100, Impulse.kr(4)).midiCps
  *   RandSeed.kr(MouseButton.kr(lag = 0) + Impulse.kr(0), 234)
  *   SinOsc.ar(freq) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.RandID$ RandID]]
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  */
object RandSeed extends ProductReader[RandSeed] {
  def ir: RandSeed = ir()
  
  /** @param trig             trigger that causes the seed to be set
    * @param seed             the seed for the random number generator, read at the
    *                         moment the trigger arrives.
    */
  def ir(trig: GE = 1, seed: GE = 56789): RandSeed = new RandSeed(scalar, trig, seed)
  
  def kr: RandSeed = kr()
  
  /** @param trig             trigger that causes the seed to be set
    * @param seed             the seed for the random number generator, read at the
    *                         moment the trigger arrives.
    */
  def kr(trig: GE = 1, seed: GE = 56789): RandSeed = new RandSeed(control, trig, seed)
  
  def read(in: RefMapIn, prefix: String, arity: Int): RandSeed = {
    require (arity == 3)
    val _rate = in.readRate()
    val _trig = in.readGE()
    val _seed = in.readGE()
    new RandSeed(_rate, _trig, _seed)
  }
}

/** A UGen that resets the seed of the synth's random number generator upon
  * receiving a trigger. All synths that use the same random number generator
  * reproduce the same sequence of numbers again. The generator can be set using the
  * `RandID` UGen.
  * 
  * @param trig             trigger that causes the seed to be set
  * @param seed             the seed for the random number generator, read at the
  *                         moment the trigger arrives.
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.RandID$ RandID]]
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  */
final case class RandSeed(rate: Rate, trig: GE = 1, seed: GE = 56789)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, seed.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** A UGen that determines which random number generator is used for the enclosing
  * synth. All synths that use the same generator reproduce the same sequence of
  * numbers when the same seed is set again.
  * 
  * ===Examples===
  * 
  * {{{
  * // button alternatingly resets left and right seed
  * 0 to 1 map { i =>
  *   play {
  *     RandID.ir(i)
  *     val x     = Impulse.kr(4)
  *     val pch   = TIRand.kr(40, 100, x)
  *     val b     = MouseButton.kr(lag = 0)
  *     val tr    = PulseDivider.kr(b, 2, i)
  *     pch.poll(x, if (i == 0) "left " else "right")
  *     RandSeed.kr(tr + Impulse.kr(0), 234)
  *     Out.ar(i, SinOsc.ar(pch.midiCps) * 0.2)
  *   }
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  */
object RandID extends ProductReader[RandID] {
  def ir: RandID = ir()
  
  /** @param id               the random number generator identifier from zero until
    *                         the maximum number of generators specified with the
    *                         server switch `-r` (default: 64)
    */
  def ir(id: GE = 0): RandID = new RandID(scalar, id)
  
  def kr: RandID = kr()
  
  /** @param id               the random number generator identifier from zero until
    *                         the maximum number of generators specified with the
    *                         server switch `-r` (default: 64)
    */
  def kr(id: GE = 0): RandID = new RandID(control, id)
  
  def read(in: RefMapIn, prefix: String, arity: Int): RandID = {
    require (arity == 2)
    val _rate = in.readRate()
    val _id   = in.readGE()
    new RandID(_rate, _id)
  }
}

/** A UGen that determines which random number generator is used for the enclosing
  * synth. All synths that use the same generator reproduce the same sequence of
  * numbers when the same seed is set again.
  * 
  * @param id               the random number generator identifier from zero until
  *                         the maximum number of generators specified with the
  *                         server switch `-r` (default: 64)
  * 
  * @see [[de.sciss.synth.ugen.Rand$ Rand]]
  * @see [[de.sciss.synth.ugen.RandSeed$ RandSeed]]
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  */
final case class RandID(rate: Rate, id: GE = 0) extends UGenSource.SingleOut with HasSideEffect {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](id.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}