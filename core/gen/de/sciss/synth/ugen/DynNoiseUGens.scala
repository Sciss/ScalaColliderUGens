// revision: 5
package de.sciss.synth
package ugen

import UGenSource._

/** A dynamic step noise UGen. Like `LFNoise0` , it generates abruptly changing
  * random values between `-1` and `+1` at a rate given by the `freq` argument, with
  * two differences: There is no time quantization, and it there is fast recovery
  * from low freq values.
  * 
  * In contrast, `LFNoise0` , `LFNoise1` , and `LFNoise2` quantize to the nearest
  * integer division of the sample rate, and they poll the freq argument only when
  * scheduled, and thus seem to hang when the frequencies get very low.
  * 
  * If very high or very low frequencies are not needed, or fixed frequencies are
  * used, `LFNoise0` is more efficient.
  * 
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFDNoise1$ LFDNoise1]]
  * @see [[de.sciss.synth.ugen.LFDNoise3$ LFDNoise3]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  */
object LFDNoise0 extends ProductType[LFDNoise0] {
  def kr: LFDNoise0 = kr()
  
  /** @param freq             rate at which to generate random values.
    */
  def kr(freq: GE = 500.0f): LFDNoise0 = new LFDNoise0(control, freq)
  
  def ar: LFDNoise0 = ar()
  
  /** @param freq             rate at which to generate random values.
    */
  def ar(freq: GE = 500.0f): LFDNoise0 = new LFDNoise0(audio, freq)
  
  final val typeId = 105
  
  def read(in: RefMapIn, key: String, arity: Int): LFDNoise0 = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFDNoise0(_rate, _freq)
  }
}

/** A dynamic step noise UGen. Like `LFNoise0` , it generates abruptly changing
  * random values between `-1` and `+1` at a rate given by the `freq` argument, with
  * two differences: There is no time quantization, and it there is fast recovery
  * from low freq values.
  * 
  * In contrast, `LFNoise0` , `LFNoise1` , and `LFNoise2` quantize to the nearest
  * integer division of the sample rate, and they poll the freq argument only when
  * scheduled, and thus seem to hang when the frequencies get very low.
  * 
  * If very high or very low frequencies are not needed, or fixed frequencies are
  * used, `LFNoise0` is more efficient.
  * 
  * @param freq             rate at which to generate random values.
  * 
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFDNoise1$ LFDNoise1]]
  * @see [[de.sciss.synth.ugen.LFDNoise3$ LFDNoise3]]
  * @see [[de.sciss.synth.ugen.TRand$ TRand]]
  */
final case class LFDNoise0(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A dynamic ramp noise UGen. Like `LFNoise1` , it generates linearly interpolated
  * random values between `-1` and `+1` at a rate given by the `freq` argument, with
  * two differences: There is no time quantization, and it there is fast recovery
  * from low freq values.
  * 
  * In contrast, `LFNoise0` , `LFNoise1` , and `LFNoise2` quantize to the nearest
  * integer division of the sample rate, and they poll the freq argument only when
  * scheduled, and thus seem to hang when the frequencies get very low.
  * 
  * If very high or very low frequencies are not needed, or fixed frequencies are
  * used, `LFNoise1` is more efficient.
  * 
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  * @see [[de.sciss.synth.ugen.LFDNoise0$ LFDNoise0]]
  * @see [[de.sciss.synth.ugen.LFDNoise3$ LFDNoise3]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
object LFDNoise1 extends ProductType[LFDNoise1] {
  def kr: LFDNoise1 = kr()
  
  /** @param freq             rate at which to generate random values.
    */
  def kr(freq: GE = 500.0f): LFDNoise1 = new LFDNoise1(control, freq)
  
  def ar: LFDNoise1 = ar()
  
  /** @param freq             rate at which to generate random values.
    */
  def ar(freq: GE = 500.0f): LFDNoise1 = new LFDNoise1(audio, freq)
  
  final val typeId = 106
  
  def read(in: RefMapIn, key: String, arity: Int): LFDNoise1 = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFDNoise1(_rate, _freq)
  }
}

/** A dynamic ramp noise UGen. Like `LFNoise1` , it generates linearly interpolated
  * random values between `-1` and `+1` at a rate given by the `freq` argument, with
  * two differences: There is no time quantization, and it there is fast recovery
  * from low freq values.
  * 
  * In contrast, `LFNoise0` , `LFNoise1` , and `LFNoise2` quantize to the nearest
  * integer division of the sample rate, and they poll the freq argument only when
  * scheduled, and thus seem to hang when the frequencies get very low.
  * 
  * If very high or very low frequencies are not needed, or fixed frequencies are
  * used, `LFNoise1` is more efficient.
  * 
  * @param freq             rate at which to generate random values.
  * 
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  * @see [[de.sciss.synth.ugen.LFDNoise0$ LFDNoise0]]
  * @see [[de.sciss.synth.ugen.LFDNoise3$ LFDNoise3]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
final case class LFDNoise1(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A dynamic ramp noise UGen. It is similar to `LFNoise2` , with three
  * differences: It uses cubic instead of quadratic interpolation for the random
  * values between `-1` and `+1` at a rate given by the `freq` argument. There is no
  * time quantization, and it there is fast recovery from low freq values.
  * 
  * In contrast, `LFNoise0` , `LFNoise1` , and `LFNoise2` quantize to the nearest
  * integer division of the sample rate, and they poll the freq argument only when
  * scheduled, and thus seem to hang when the frequencies get very low.
  * 
  * ===Examples===
  * 
  * {{{
  * // compare dynamic and non-dynamic
  * play {
  *   val r    = MouseX.kr(0.1, 1000, 1)
  *   val a    = LFNoise2 .ar(r)
  *   val b    = LFDNoise3.ar(r)
  *   val freq = Select.ar(MouseButton.kr(lag = 0), Seq(a, b))
  *   SinOsc.ar(freq.mulAdd(200, 500)) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFNoise2$ LFNoise2]]
  * @see [[de.sciss.synth.ugen.LFDNoise0$ LFDNoise0]]
  * @see [[de.sciss.synth.ugen.LFDNoise1$ LFDNoise1]]
  */
object LFDNoise3 extends ProductType[LFDNoise3] {
  def kr: LFDNoise3 = kr()
  
  /** @param freq             rate at which to generate random values.
    */
  def kr(freq: GE = 500.0f): LFDNoise3 = new LFDNoise3(control, freq)
  
  def ar: LFDNoise3 = ar()
  
  /** @param freq             rate at which to generate random values.
    */
  def ar(freq: GE = 500.0f): LFDNoise3 = new LFDNoise3(audio, freq)
  
  final val typeId = 107
  
  def read(in: RefMapIn, key: String, arity: Int): LFDNoise3 = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFDNoise3(_rate, _freq)
  }
}

/** A dynamic ramp noise UGen. It is similar to `LFNoise2` , with three
  * differences: It uses cubic instead of quadratic interpolation for the random
  * values between `-1` and `+1` at a rate given by the `freq` argument. There is no
  * time quantization, and it there is fast recovery from low freq values.
  * 
  * In contrast, `LFNoise0` , `LFNoise1` , and `LFNoise2` quantize to the nearest
  * integer division of the sample rate, and they poll the freq argument only when
  * scheduled, and thus seem to hang when the frequencies get very low.
  * 
  * @param freq             rate at which to generate random values.
  * 
  * @see [[de.sciss.synth.ugen.LFNoise2$ LFNoise2]]
  * @see [[de.sciss.synth.ugen.LFDNoise0$ LFDNoise0]]
  * @see [[de.sciss.synth.ugen.LFDNoise1$ LFDNoise1]]
  */
final case class LFDNoise3(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen that randomly generates the values -1 or +1 at a rate given by the
  * nearest integer division of the sample rate by the frequency argument. The
  * difference to `LFClipNoise` is that this UGen does not quantize time and
  * recovers fast from frequency input changes.
  * 
  * ===Examples===
  * 
  * {{{
  * // generator
  * play { LFDClipNoise.ar(500) * 0.2 }
  * }}}
  * {{{
  * // random panning
  * play {
  *   val pos = LFDClipNoise.ar(4)
  *   Pan2.ar(PinkNoise.ar, pos)
  * }
  * }}}
  * {{{
  * // modulate frequency
  * play { LFDClipNoise.ar(XLine.kr(100, 10000, 20)) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFClipNoise$ LFClipNoise]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  * @see [[de.sciss.synth.ugen.CoinGate$ CoinGate]]
  * @see [[de.sciss.synth.ugen.Schmidt$ Schmidt]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  */
object LFDClipNoise extends ProductType[LFDClipNoise] {
  def kr: LFDClipNoise = kr()
  
  /** @param freq             rate at which to generate random values.
    */
  def kr(freq: GE = 500.0f): LFDClipNoise = new LFDClipNoise(control, freq)
  
  def ar: LFDClipNoise = ar()
  
  /** @param freq             rate at which to generate random values.
    */
  def ar(freq: GE = 500.0f): LFDClipNoise = new LFDClipNoise(audio, freq)
  
  final val typeId = 108
  
  def read(in: RefMapIn, key: String, arity: Int): LFDClipNoise = {
    require (arity == 2)
    val _rate = in.readRate()
    val _freq = in.readGE()
    new LFDClipNoise(_rate, _freq)
  }
}

/** A UGen that randomly generates the values -1 or +1 at a rate given by the
  * nearest integer division of the sample rate by the frequency argument. The
  * difference to `LFClipNoise` is that this UGen does not quantize time and
  * recovers fast from frequency input changes.
  * 
  * @param freq             rate at which to generate random values.
  * 
  * @see [[de.sciss.synth.ugen.LFClipNoise$ LFClipNoise]]
  * @see [[de.sciss.synth.ugen.LFNoise0$ LFNoise0]]
  * @see [[de.sciss.synth.ugen.LFNoise1$ LFNoise1]]
  * @see [[de.sciss.synth.ugen.CoinGate$ CoinGate]]
  * @see [[de.sciss.synth.ugen.Schmidt$ Schmidt]]
  * @see [[de.sciss.synth.ugen.GrayNoise$ GrayNoise]]
  */
final case class LFDClipNoise(rate: Rate, freq: GE = 500.0f) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}