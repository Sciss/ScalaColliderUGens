// revision: 11
package de.sciss.synth
package ugen

import UGenSource._

/** An algorithmic reverb UGen, inspired by the lush chorused sound of certain
  * vintage Lexicon and Alesis reverberation units. Designed to sound great with
  * synthetic sound sources, rather than sound like a realistic space.
  * 
  * ''Note'': You may need to increase the server's real-time memory
  * 
  * ===Examples===
  * 
  * {{{
  * // defaults
  * play {
  *   val src = SplayAz.ar(2, Impulse.ar(Seq(1, 3, 5, 7, 9)))
  *   JPverb.ar(inL = src.out(0), inR = src.out(1))
  * }
  * }}}
  * {{{
  * // dream-verb
  * play {
  *   val src = SplayAz.ar(2, Impulse.ar(Seq(1, 3, 5, 7, 9)))
  *   0.4 * src + 0.8 * JPverb.ar(
  *     inL = src.out(0), inR = src.out(1), revTime = 60, size = 2.8, damp = 0.3, earlyDiff = 0.42,
  *     low = 0.84, mid = 0.71, high = 0.0,
  *     lowCut = 2450, highCut = 1024, modFreq = 0.1, modDepth = 4.6)
  * }
  * }}}
  * {{{
  * // tail modulation
  * play {
  *   val src = SplayAz.ar(2, Impulse.ar(Seq(1, 3, 5, 7, 9)))
  *   val time = LFSaw.ar(0.02).linExp(-1, 1, 0.02, 60)
  *   JPverb.ar(
  *     inL = src.out(0), inR = src.out(1), revTime = time, size = 1.0, damp = 0.3, earlyDiff = 0.0,
  *     low = 1, mid = 0, high = 1,
  *     lowCut = 2450, highCut = 1024, modDepth = 0)
  * }
  * }}}
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb2$ FreeVerb2]]
  * @see [[de.sciss.synth.ugen.GVerb$ GVerb]]
  * @see [[de.sciss.synth.ugen.Greyhole$ Greyhole]]
  */
object JPverb extends ProductReader[JPverb] {
  /** @param inL              left input signal to be reverberated
    * @param inR              right input signal to be reverberated
    * @param revTime          approximate reverberation time in seconds (T60 - the
    *                         time for the reverberation to decay 60 dB). Does not
    *                         effect early reflections. (0.1..60)
    * @param damp             damping of high-frequencies as the reverberation
    *                         decays. 0 is no damping, 1 is very strong damping (0..1)
    * @param size             scales the size of delay-lines, producing the
    *                         impression of a larger or smaller space. Values below 1
    *                         can sound quite metallic. (0.5..5)
    * @param earlyDiff        shape of early reflections. Values of > 0.707 produce
    *                         smooth exponential decay. Lower values produce a slower
    *                         build-up of echoes. (0..1)
    * @param modDepth         depth of delay-line modulation in sample frames. Use in
    *                         combination with `modFreq` to set amount of chorusing
    *                         within the structure. (0..50)
    * @param modFreq          frequency of delay-line modulation. Use in combination
    *                         with `modDepth` to set amount of chorusing within the
    *                         structure. (0..10)
    * @param low              multiplier for the reverberation time within the low
    *                         band. (0..1)
    * @param mid              multiplier for the reverberation time within the mid
    *                         band. (0..1)
    * @param high             multiplier for the reverberation time within the high
    *                         band. (0..1)
    * @param lowCut           frequency in Hz at which the crossover between the low
    *                         and mid bands of the reverberation occurs. (100..6000)
    * @param highCut          frequency in Hz at which the crossover between the mid
    *                         and high bands of the reverberation occurs.
    *                         (1000..10000)
    */
  def kr(inL: GE, inR: GE, revTime: GE = 1.0f, damp: GE = 0.0f, size: GE = 1.0f, earlyDiff: GE = 0.707f, modDepth: GE = 0.1f, modFreq: GE = 2.0f, low: GE = 1.0f, mid: GE = 1.0f, high: GE = 1.0f, lowCut: GE = 500.0f, highCut: GE = 2000.0f): JPverb = 
    new JPverb(control, inL, inR, revTime, damp, size, earlyDiff, modDepth, modFreq, low, mid, high, lowCut, highCut)
  
  /** @param inL              left input signal to be reverberated
    * @param inR              right input signal to be reverberated
    * @param revTime          approximate reverberation time in seconds (T60 - the
    *                         time for the reverberation to decay 60 dB). Does not
    *                         effect early reflections. (0.1..60)
    * @param damp             damping of high-frequencies as the reverberation
    *                         decays. 0 is no damping, 1 is very strong damping (0..1)
    * @param size             scales the size of delay-lines, producing the
    *                         impression of a larger or smaller space. Values below 1
    *                         can sound quite metallic. (0.5..5)
    * @param earlyDiff        shape of early reflections. Values of > 0.707 produce
    *                         smooth exponential decay. Lower values produce a slower
    *                         build-up of echoes. (0..1)
    * @param modDepth         depth of delay-line modulation in sample frames. Use in
    *                         combination with `modFreq` to set amount of chorusing
    *                         within the structure. (0..50)
    * @param modFreq          frequency of delay-line modulation. Use in combination
    *                         with `modDepth` to set amount of chorusing within the
    *                         structure. (0..10)
    * @param low              multiplier for the reverberation time within the low
    *                         band. (0..1)
    * @param mid              multiplier for the reverberation time within the mid
    *                         band. (0..1)
    * @param high             multiplier for the reverberation time within the high
    *                         band. (0..1)
    * @param lowCut           frequency in Hz at which the crossover between the low
    *                         and mid bands of the reverberation occurs. (100..6000)
    * @param highCut          frequency in Hz at which the crossover between the mid
    *                         and high bands of the reverberation occurs.
    *                         (1000..10000)
    */
  def ar(inL: GE, inR: GE, revTime: GE = 1.0f, damp: GE = 0.0f, size: GE = 1.0f, earlyDiff: GE = 0.707f, modDepth: GE = 0.1f, modFreq: GE = 2.0f, low: GE = 1.0f, mid: GE = 1.0f, high: GE = 1.0f, lowCut: GE = 500.0f, highCut: GE = 2000.0f): JPverb = 
    new JPverb(audio, inL, inR, revTime, damp, size, earlyDiff, modDepth, modFreq, low, mid, high, lowCut, highCut)
  
  def read(in: RefMapIn, prefix: String, arity: Int): JPverb = {
    require (arity == 14)
    val _rate       = in.readRate()
    val _inL        = in.readGE()
    val _inR        = in.readGE()
    val _revTime    = in.readGE()
    val _damp       = in.readGE()
    val _size       = in.readGE()
    val _earlyDiff  = in.readGE()
    val _modDepth   = in.readGE()
    val _modFreq    = in.readGE()
    val _low        = in.readGE()
    val _mid        = in.readGE()
    val _high       = in.readGE()
    val _lowCut     = in.readGE()
    val _highCut    = in.readGE()
    new JPverb(_rate, _inL, _inR, _revTime, _damp, _size, _earlyDiff, _modDepth, _modFreq, _low, _mid, _high, _lowCut, _highCut)
  }
}

/** An algorithmic reverb UGen, inspired by the lush chorused sound of certain
  * vintage Lexicon and Alesis reverberation units. Designed to sound great with
  * synthetic sound sources, rather than sound like a realistic space.
  * 
  * ''Note'': You may need to increase the server's real-time memory
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @param inL              left input signal to be reverberated
  * @param inR              right input signal to be reverberated
  * @param revTime          approximate reverberation time in seconds (T60 - the
  *                         time for the reverberation to decay 60 dB). Does not
  *                         effect early reflections. (0.1..60)
  * @param damp             damping of high-frequencies as the reverberation
  *                         decays. 0 is no damping, 1 is very strong damping (0..1)
  * @param size             scales the size of delay-lines, producing the
  *                         impression of a larger or smaller space. Values below 1
  *                         can sound quite metallic. (0.5..5)
  * @param earlyDiff        shape of early reflections. Values of > 0.707 produce
  *                         smooth exponential decay. Lower values produce a slower
  *                         build-up of echoes. (0..1)
  * @param modDepth         depth of delay-line modulation in sample frames. Use in
  *                         combination with `modFreq` to set amount of chorusing
  *                         within the structure. (0..50)
  * @param modFreq          frequency of delay-line modulation. Use in combination
  *                         with `modDepth` to set amount of chorusing within the
  *                         structure. (0..10)
  * @param low              multiplier for the reverberation time within the low
  *                         band. (0..1)
  * @param mid              multiplier for the reverberation time within the mid
  *                         band. (0..1)
  * @param high             multiplier for the reverberation time within the high
  *                         band. (0..1)
  * @param lowCut           frequency in Hz at which the crossover between the low
  *                         and mid bands of the reverberation occurs. (100..6000)
  * @param highCut          frequency in Hz at which the crossover between the mid
  *                         and high bands of the reverberation occurs.
  *                         (1000..10000)
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb2$ FreeVerb2]]
  * @see [[de.sciss.synth.ugen.GVerb$ GVerb]]
  * @see [[de.sciss.synth.ugen.Greyhole$ Greyhole]]
  */
final case class JPverb(rate: Rate, inL: GE, inR: GE, revTime: GE = 1.0f, damp: GE = 0.0f, size: GE = 1.0f, earlyDiff: GE = 0.707f, modDepth: GE = 0.1f, modFreq: GE = 2.0f, low: GE = 1.0f, mid: GE = 1.0f, high: GE = 1.0f, lowCut: GE = 500.0f, highCut: GE = 2000.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inL.expand, inR.expand, damp.expand, earlyDiff.expand, highCut.expand, high.expand, lowCut.expand, low.expand, modDepth.expand, modFreq.expand, mid.expand, size.expand, revTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.MultiOut("JPverbRaw", rate, Vector.fill(2)(rate), _args2)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** A complex echo-like effect UGen, inspired by the classic Eventide effect of a
  * similar name. The effect consists of a diffuser (like a mini-reverb) connected
  * in a feedback system with a long modulated delay-line. Excels at producing
  * spacey washes of sound.
  * 
  * ''Note'': You may need to increase the server's real-time memory
  * 
  * ===Examples===
  * 
  * {{{
  * // discrete
  * play {
  *   val src = LeakDC.ar(SplayAz.ar(2, Impulse.ar(Seq(1, 3, 5, 7, 9))))
  *   Greyhole.ar(
  *     inL = src.out(0), inR = src.out(1), delayTime = 0.1, damp = 0.1,
  *     feedback = 0.1, modDepth = 0.01, modFreq = 2)
  * }
  * }}}
  * {{{
  * // time modulation
  * play {
  *   val src = LeakDC.ar(SplayAz.ar(2, Impulse.ar(Seq(1, 3, 5, 7, 9))))
  *   val time = LFTri.kr(0.01).linExp(-1, 1, 0.05, 0.2)
  *   Greyhole.ar(
  *     inL = src.out(0), inR = src.out(1), delayTime = time, damp = 0.4,
  *     feedback = 0.99, modDepth = 0.01, modFreq = 2)
  * }
  * }}}
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @see [[de.sciss.synth.ugen.JPverb$ JPverb]]
  */
object Greyhole extends ProductReader[Greyhole] {
  /** @param inL              left input signal
    * @param inR              right input signal
    * @param delayTime        approximate delay time in seconds. (0.1..60)
    * @param damp             damping of high-frequencies as the delay decays. 0 is
    *                         no damping, 1 is very strong damping (0..1)
    * @param size             scales the size of delay-lines, producing the
    *                         impression of a larger or smaller space. Values below 1
    *                         can sound quite metallic. (0.5..5)
    * @param diff             shape of echo patterns produced by the diffuser. At
    *                         very low values, the diffuser acts like a delay-line
    *                         whose length is controlled by the `size` parameter.
    *                         Medium values produce a slow build-up of echoes, giving
    *                         the sound a reversed-like quality. Values of 0.707 or
    *                         greater than produce smooth exponentially decaying
    *                         echoes. (0..1)
    * @param feedback         amount of feedback through the system. Sets the number
    *                         of repeating echoes. A setting of 1.0 produces infinite
    *                         sustain. (0..1)
    * @param modDepth         depth of delay-line modulation. Use in combination with
    *                         `modFreq` to produce chorus and pitch-variations in the
    *                         echoes. (0..1)
    * @param modFreq          frequency of delay-line modulation. Use in combination
    *                         with `modDepth` to produce chorus and pitch-variations
    *                         in the echoes. (0..10)
    */
  def ar(inL: GE, inR: GE, delayTime: GE = 2.0f, damp: GE = 0.0f, size: GE = 1.0f, diff: GE = 0.707f, feedback: GE = 0.9f, modDepth: GE = 0.1f, modFreq: GE = 2.0f): Greyhole = 
    new Greyhole(audio, inL, inR, delayTime, damp, size, diff, feedback, modDepth, modFreq)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Greyhole = {
    require (arity == 10)
    val _rate       = in.readRate()
    val _inL        = in.readGE()
    val _inR        = in.readGE()
    val _delayTime  = in.readGE()
    val _damp       = in.readGE()
    val _size       = in.readGE()
    val _diff       = in.readGE()
    val _feedback   = in.readGE()
    val _modDepth   = in.readGE()
    val _modFreq    = in.readGE()
    new Greyhole(_rate, _inL, _inR, _delayTime, _damp, _size, _diff, _feedback, _modDepth, _modFreq)
  }
}

/** A complex echo-like effect UGen, inspired by the classic Eventide effect of a
  * similar name. The effect consists of a diffuser (like a mini-reverb) connected
  * in a feedback system with a long modulated delay-line. Excels at producing
  * spacey washes of sound.
  * 
  * ''Note'': You may need to increase the server's real-time memory
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @param inL              left input signal
  * @param inR              right input signal
  * @param delayTime        approximate delay time in seconds. (0.1..60)
  * @param damp             damping of high-frequencies as the delay decays. 0 is
  *                         no damping, 1 is very strong damping (0..1)
  * @param size             scales the size of delay-lines, producing the
  *                         impression of a larger or smaller space. Values below 1
  *                         can sound quite metallic. (0.5..5)
  * @param diff             shape of echo patterns produced by the diffuser. At
  *                         very low values, the diffuser acts like a delay-line
  *                         whose length is controlled by the `size` parameter.
  *                         Medium values produce a slow build-up of echoes, giving
  *                         the sound a reversed-like quality. Values of 0.707 or
  *                         greater than produce smooth exponentially decaying
  *                         echoes. (0..1)
  * @param feedback         amount of feedback through the system. Sets the number
  *                         of repeating echoes. A setting of 1.0 produces infinite
  *                         sustain. (0..1)
  * @param modDepth         depth of delay-line modulation. Use in combination with
  *                         `modFreq` to produce chorus and pitch-variations in the
  *                         echoes. (0..1)
  * @param modFreq          frequency of delay-line modulation. Use in combination
  *                         with `modDepth` to produce chorus and pitch-variations
  *                         in the echoes. (0..10)
  * 
  * @see [[de.sciss.synth.ugen.JPverb$ JPverb]]
  */
final case class Greyhole(rate: Rate, inL: GE, inR: GE, delayTime: GE = 2.0f, damp: GE = 0.0f, size: GE = 1.0f, diff: GE = 0.707f, feedback: GE = 0.9f, modDepth: GE = 0.1f, modFreq: GE = 2.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inL.expand, inR.expand, damp.expand, delayTime.expand, diff.expand, feedback.expand, modDepth.expand, modFreq.expand, size.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.MultiOut("GreyholeRaw", rate, Vector.fill(2)(rate), _args2)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** A resonating filter UGen which can be modulated in its resonating frequency at
  * audio rate.
  * 
  * Implements the filter structure found in Julian Parker and Till Bovermann
  * (2013): ''Dynamic FM synthesis using a network of complex resonator filters''
  * 
  * ===Examples===
  * 
  * {{{
  * // pulse excitation
  * play { ComplexRes.ar(Pulse.ar(1, 0.01), 5000 * SinOsc.ar(Seq(50, 51)), 0.5) }
  * }}}
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.Formlet$ Formlet]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  */
object ComplexRes extends ProductReader[ComplexRes] {
  /** @param in               input signal to be filtered
    * @param freq             resonating frequency in Hz, can be modulated at audio
    *                         rate
    * @param decay            decay time in seconds
    */
  def ar(in: GE, freq: GE = 440.0f, decay: GE = 0.2f): ComplexRes = 
    new ComplexRes(audio, in, freq, decay)
  
  def read(in: RefMapIn, prefix: String, arity: Int): ComplexRes = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _decay  = in.readGE()
    new ComplexRes(_rate, _in, _freq, _decay)
  }
}

/** A resonating filter UGen which can be modulated in its resonating frequency at
  * audio rate.
  * 
  * Implements the filter structure found in Julian Parker and Till Bovermann
  * (2013): ''Dynamic FM synthesis using a network of complex resonator filters''
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @param in               input signal to be filtered
  * @param freq             resonating frequency in Hz, can be modulated at audio
  *                         rate
  * @param decay            decay time in seconds
  * 
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.Formlet$ Formlet]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  */
final case class ComplexRes(rate: Rate, in: GE, freq: GE = 440.0f, decay: GE = 0.2f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, decay.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** Ring modulation UGen based on a physical model of a diode.
  * 
  * ===Examples===
  * 
  * {{{
  * // sprinkle
  * play {
  *   val ring = DiodeRingMod.ar(
  *     SinOsc.ar((3700: GE) * Seq(1.0, 1.1, 1.2) * (SinOsc.ar(200) + 2)),
  *     SinOsc.ar(( 100: GE) * Seq(0.75, 1, 0.5)))
  *   SplayAz.ar(2, ring) * 0.2 * LFPulse.ar(10.3 * 0.5, 0, 0.04) * 0.5
  * }
  * }}}
  * {{{
  * // wobble
  * play {
  *   val ring = DiodeRingMod.ar(
  *     SinOsc.ar((400: GE) * Seq(1.0, 1.1, 1.2) * (SinOsc.ar(200) + 2)),
  *     SinOsc.ar((100: GE) * Seq(0.75, 1, 0.5)))
  *   SplayAz.ar(2, ring) * 0.2 * LFPulse.ar(10.3 * 1/32, 0, 0.2) * 0.25
  * }
  * }}}
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  */
object DiodeRingMod extends ProductReader[DiodeRingMod] {
  /** @param car              carrier signal
    * @param mod              modulator signal
    */
  def ar(car: GE, mod: GE): DiodeRingMod = new DiodeRingMod(audio, car, mod)
  
  def read(in: RefMapIn, prefix: String, arity: Int): DiodeRingMod = {
    require (arity == 3)
    val _rate = in.readRate()
    val _car  = in.readGE()
    val _mod  = in.readGE()
    new DiodeRingMod(_rate, _car, _mod)
  }
}

/** Ring modulation UGen based on a physical model of a diode.
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @param car              carrier signal
  * @param mod              modulator signal
  * 
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  */
final case class DiodeRingMod(rate: Rate, car: GE, mod: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](car.expand, mod.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.SingleOut(name, rate, _args2)
  }
}

/** Demand rate UGen implementing a Wiard noise ring.
  * 
  * "In latter model synthesizers, digital noise sources began to appear in place
  * of analog ones. Traditionally, a pseudo-random shift register set up for optimal
  * length. By optimal length, it is meant that every state of all available bits
  * will appear at some time, but the order is unknown. Essentially a counter that
  * counts in an unknown order. This represents the maximum state of information
  * "entropy" available for that number of bits. But music has close self-similarity
  * over short periods of time. That is, it repeats itself with changes appearing
  * slowly. This shift register generator is designed to give control of the rate of
  * appearance of new information. It has a tight set of controls over how random it
  * actually is and how fast change occurs." (source:
  * http://mamonu.weebly.com/wiard-noisering.html)
  * 
  * ===Examples===
  * 
  * {{{
  * // plain 32bit value scaled to 0..1
  * play {
  *   val noise = DNoiseRing(change = MouseX.kr(0, 1), chance = 0.51, numBits = 32)
  *   Demand.ar(Impulse.ar(10000), noise) / 2.0.pow(33)
  * }
  * }}}
  * {{{
  * // sequencer
  * play {
  *   val noise = DNoiseRing(change = MouseX.kr(0, 1), chance = MouseY.kr(0, 1), numBits = 32)
  *   val tr    = Impulse.ar(10)
  *   val freq  = (Demand.ar(tr, noise)).linLin(0, 2.0.pow(32), 40, 40+48).midiCps
  *   freq.poll(tr, "freq")
  *   Pan2.ar(SinOsc.ar(freq) * 0.25)
  * }
  * }}}
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object DNoiseRing extends ProductReader[DNoiseRing] {
  def read(in: RefMapIn, prefix: String, arity: Int): DNoiseRing = {
    require (arity == 5)
    val _change   = in.readGE()
    val _chance   = in.readGE()
    val _shift    = in.readGE()
    val _numBits  = in.readGE()
    val _init     = in.readGE()
    new DNoiseRing(_change, _chance, _shift, _numBits, _init)
  }
}

/** Demand rate UGen implementing a Wiard noise ring.
  * 
  * "In latter model synthesizers, digital noise sources began to appear in place
  * of analog ones. Traditionally, a pseudo-random shift register set up for optimal
  * length. By optimal length, it is meant that every state of all available bits
  * will appear at some time, but the order is unknown. Essentially a counter that
  * counts in an unknown order. This represents the maximum state of information
  * "entropy" available for that number of bits. But music has close self-similarity
  * over short periods of time. That is, it repeats itself with changes appearing
  * slowly. This shift register generator is designed to give control of the rate of
  * appearance of new information. It has a tight set of controls over how random it
  * actually is and how fast change occurs." (source:
  * http://mamonu.weebly.com/wiard-noisering.html)
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @param change           probability of changing to a new value
  * @param chance           probability of the new value becoming HIGH
  * @param init             initial internal state
  * 
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class DNoiseRing(change: GE = 0.5f, chance: GE = 0.5f, shift: GE = 1, numBits: GE = 8, init: GE = 0)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](change.expand, chance.expand, shift.expand, numBits.expand, init.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A UGen that calculates the root-mean-square of a first order low-pass filtered
  * input signal. The formula is 'rms = sqrt(lpf1(x^2))'.
  * 
  * ===Examples===
  * 
  * {{{
  * // measure mouse-controlled sine
  * play {
  *   // with MouseX at maximum, you'll see that the sine has -3 dB RMS
  *   val sig = SinOsc.ar(300) * MouseX.kr(0, 1) * LFPulse.ar(0.5)
  *   val rms = RMS.ar(sig, 10).ampDb.roundTo(0.1).poll(8, "RMS (dB)")
  *   sig
  * }
  * }}}
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  */
object RMS extends ProductReader[RMS] {
  /** @param in               input signal to be analyzed
    * @param lpf              low-pass filter frequency in Hz
    */
  def kr(in: GE, lpf: GE = 2.0f): RMS = new RMS(control, in, lpf)
  
  /** @param in               input signal to be analyzed
    * @param lpf              low-pass filter frequency in Hz
    */
  def ar(in: GE, lpf: GE = 2.0f): RMS = new RMS(audio, in, lpf)
  
  def read(in: RefMapIn, prefix: String, arity: Int): RMS = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _lpf  = in.readGE()
    new RMS(_rate, _in, _lpf)
  }
}

/** A UGen that calculates the root-mean-square of a first order low-pass filtered
  * input signal. The formula is 'rms = sqrt(lpf1(x^2))'.
  * 
  * This is a third-party UGen (DEINDUGens).
  * 
  * @param in               input signal to be analyzed
  * @param lpf              low-pass filter frequency in Hz
  * 
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  */
final case class RMS(rate: Rate, in: GE, lpf: GE = 2.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, lpf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}