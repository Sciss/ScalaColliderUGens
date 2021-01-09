// revision: 8
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen which produces a linear lag (time smear) regarding and input signal.
  * Other than `Lag` which is a feedback filter with exponential decay, `Ramp`
  * applies a linear ramp. This is achieved by sampling the input signal at regular
  * intervals given by the `lagTime` and starting a new line segment after each
  * interval.
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Sweep$ Sweep]]
  */
object Ramp extends ProductReader[Ramp] {
  /** @param in               the signal to smooth out
    * @param dur              the ramp-time (seconds) which is also the interval of
    *                         the sampling
    */
  def kr(in: GE, dur: GE = 0.1f): Ramp = new Ramp(control, in, dur)
  
  /** @param in               the signal to smooth out
    * @param dur              the ramp-time (seconds) which is also the interval of
    *                         the sampling
    */
  def ar(in: GE, dur: GE = 0.1f): Ramp = new Ramp(audio, in, dur)
  
  def read(in: RefMapIn, arity: Int): Ramp = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _dur  = in.readGE()
    new Ramp(_rate, _in, _dur)
  }
}

/** A UGen which produces a linear lag (time smear) regarding and input signal.
  * Other than `Lag` which is a feedback filter with exponential decay, `Ramp`
  * applies a linear ramp. This is achieved by sampling the input signal at regular
  * intervals given by the `lagTime` and starting a new line segment after each
  * interval.
  * 
  * @param in               the signal to smooth out
  * @param dur              the ramp-time (seconds) which is also the interval of
  *                         the sampling
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Sweep$ Sweep]]
  */
final case class Ramp(rate: MaybeRate, in: GE, dur: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, dur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** An exponential lag UGen. This is essentially the same as `OnePole` except that
  * instead of supplying the coefficient directly, it is calculated from a 60 dB lag
  * time. This is the time required for the filter to converge to within 0.01 % of a
  * value. This is useful for smoothing out control signals.
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  * @see [[de.sciss.synth.ugen.Lag2$ Lag2]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
object Lag extends ProductReader[Lag] {
  /** @param in               input signal.
    * @param time             60 dB lag time in seconds.
    */
  def kr(in: GE, time: GE = 0.1f): Lag = new Lag(control, in, time)
  
  /** @param in               input signal.
    * @param time             60 dB lag time in seconds.
    */
  def ar(in: GE, time: GE = 0.1f): Lag = new Lag(audio, in, time)
  
  def read(in: RefMapIn, arity: Int): Lag = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _time = in.readGE()
    new Lag(_rate, _in, _time)
  }
}

/** An exponential lag UGen. This is essentially the same as `OnePole` except that
  * instead of supplying the coefficient directly, it is calculated from a 60 dB lag
  * time. This is the time required for the filter to converge to within 0.01 % of a
  * value. This is useful for smoothing out control signals.
  * 
  * @param in               input signal.
  * @param time             60 dB lag time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  * @see [[de.sciss.synth.ugen.Lag2$ Lag2]]
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  */
final case class Lag(rate: MaybeRate, in: GE, time: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, time.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A cascaded exponential lag UGen. `Lag2.kr(in, time)` is equivalent to
  * `Lag.kr(Lag.kr(in, time), time)` , thus resulting in a smoother transition. This
  * saves on CPU as you only have to calculate the decay factor once instead of
  * twice.
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Lag2UD$ Lag2UD]]
  * @see [[de.sciss.synth.ugen.Lag3$ Lag3]]
  */
object Lag2 extends ProductReader[Lag2] {
  /** @param in               input signal.
    * @param time             60 dB lag time in seconds.
    */
  def kr(in: GE, time: GE = 0.1f): Lag2 = new Lag2(control, in, time)
  
  /** @param in               input signal.
    * @param time             60 dB lag time in seconds.
    */
  def ar(in: GE, time: GE = 0.1f): Lag2 = new Lag2(audio, in, time)
  
  def read(in: RefMapIn, arity: Int): Lag2 = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _time = in.readGE()
    new Lag2(_rate, _in, _time)
  }
}

/** A cascaded exponential lag UGen. `Lag2.kr(in, time)` is equivalent to
  * `Lag.kr(Lag.kr(in, time), time)` , thus resulting in a smoother transition. This
  * saves on CPU as you only have to calculate the decay factor once instead of
  * twice.
  * 
  * @param in               input signal.
  * @param time             60 dB lag time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Lag2UD$ Lag2UD]]
  * @see [[de.sciss.synth.ugen.Lag3$ Lag3]]
  */
final case class Lag2(rate: MaybeRate, in: GE, time: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, time.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A cascaded exponential lag UGen. `Lag3.kr(in, time)` is equivalent to
  * `Lag.kr(Lag.kr(Lag.kr(Lag.kr(in, time), time), time)` , thus resulting in a
  * smoother transition. This saves on CPU as you only have to calculate the decay
  * factor once instead of three times.
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Lag3UD$ Lag3UD]]
  * @see [[de.sciss.synth.ugen.Lag2$ Lag2]]
  */
object Lag3 extends ProductReader[Lag3] {
  /** @param in               input signal.
    * @param time             60 dB lag time in seconds.
    */
  def kr(in: GE, time: GE = 0.1f): Lag3 = new Lag3(control, in, time)
  
  /** @param in               input signal.
    * @param time             60 dB lag time in seconds.
    */
  def ar(in: GE, time: GE = 0.1f): Lag3 = new Lag3(audio, in, time)
  
  def read(in: RefMapIn, arity: Int): Lag3 = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _time = in.readGE()
    new Lag3(_rate, _in, _time)
  }
}

/** A cascaded exponential lag UGen. `Lag3.kr(in, time)` is equivalent to
  * `Lag.kr(Lag.kr(Lag.kr(Lag.kr(in, time), time), time)` , thus resulting in a
  * smoother transition. This saves on CPU as you only have to calculate the decay
  * factor once instead of three times.
  * 
  * @param in               input signal.
  * @param time             60 dB lag time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Lag3UD$ Lag3UD]]
  * @see [[de.sciss.synth.ugen.Lag2$ Lag2]]
  */
final case class Lag3(rate: MaybeRate, in: GE, time: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, time.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** An exponential lag UGen with separate inputs for up and down slope. This is
  * essentially the same as `Lag` except that you can supply a different 60 dB time
  * for when the signal goes up, from when the signal goes down.
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Lag2UD$ Lag2UD]]
  */
object LagUD extends ProductReader[LagUD] {
  /** @param in               input signal.
    * @param timeUp           60 dB lag time in seconds effective during a rising
    *                         slope in the input signal
    * @param timeDown         60 dB lag time in seconds effective during a falling
    *                         slope in the input signal
    */
  def kr(in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f): LagUD = 
    new LagUD(control, in, timeUp, timeDown)
  
  /** @param in               input signal.
    * @param timeUp           60 dB lag time in seconds effective during a rising
    *                         slope in the input signal
    * @param timeDown         60 dB lag time in seconds effective during a falling
    *                         slope in the input signal
    */
  def ar(in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f): LagUD = new LagUD(audio, in, timeUp, timeDown)
  
  def read(in: RefMapIn, arity: Int): LagUD = {
    require (arity == 4)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _timeUp   = in.readGE()
    val _timeDown = in.readGE()
    new LagUD(_rate, _in, _timeUp, _timeDown)
  }
}

/** An exponential lag UGen with separate inputs for up and down slope. This is
  * essentially the same as `Lag` except that you can supply a different 60 dB time
  * for when the signal goes up, from when the signal goes down.
  * 
  * @param in               input signal.
  * @param timeUp           60 dB lag time in seconds effective during a rising
  *                         slope in the input signal
  * @param timeDown         60 dB lag time in seconds effective during a falling
  *                         slope in the input signal
  * 
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  * @see [[de.sciss.synth.ugen.Lag2UD$ Lag2UD]]
  */
final case class LagUD(rate: MaybeRate, in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, timeUp.expand, timeDown.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A cascaded exponential lag UGen with separate inputs for up and down slope.
  * `Lag2UD.kr(in, up, down)` is equivalent to
  * `LagUD.kr(LagUD.kr(in, up, down), up, down)` , thus resulting in a smoother
  * transition. This saves on CPU as you only have to calculate the decay factors
  * once instead of twice.
  * 
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  * @see [[de.sciss.synth.ugen.Lag2$ Lag2]]
  * @see [[de.sciss.synth.ugen.Lag3UD$ Lag3UD]]
  */
object Lag2UD extends ProductReader[Lag2UD] {
  /** @param in               input signal.
    * @param timeUp           60 dB lag time in seconds effective during a rising
    *                         slope in the input signal
    * @param timeDown         60 dB lag time in seconds effective during a falling
    *                         slope in the input signal
    */
  def kr(in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f): Lag2UD = 
    new Lag2UD(control, in, timeUp, timeDown)
  
  /** @param in               input signal.
    * @param timeUp           60 dB lag time in seconds effective during a rising
    *                         slope in the input signal
    * @param timeDown         60 dB lag time in seconds effective during a falling
    *                         slope in the input signal
    */
  def ar(in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f): Lag2UD = 
    new Lag2UD(audio, in, timeUp, timeDown)
  
  def read(in: RefMapIn, arity: Int): Lag2UD = {
    require (arity == 4)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _timeUp   = in.readGE()
    val _timeDown = in.readGE()
    new Lag2UD(_rate, _in, _timeUp, _timeDown)
  }
}

/** A cascaded exponential lag UGen with separate inputs for up and down slope.
  * `Lag2UD.kr(in, up, down)` is equivalent to
  * `LagUD.kr(LagUD.kr(in, up, down), up, down)` , thus resulting in a smoother
  * transition. This saves on CPU as you only have to calculate the decay factors
  * once instead of twice.
  * 
  * @param in               input signal.
  * @param timeUp           60 dB lag time in seconds effective during a rising
  *                         slope in the input signal
  * @param timeDown         60 dB lag time in seconds effective during a falling
  *                         slope in the input signal
  * 
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  * @see [[de.sciss.synth.ugen.Lag2$ Lag2]]
  * @see [[de.sciss.synth.ugen.Lag3UD$ Lag3UD]]
  */
final case class Lag2UD(rate: MaybeRate, in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, timeUp.expand, timeDown.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A cascaded exponential lag UGen with separate inputs for up and down slope.
  * `Lag3UD.kr(in, up, down)` is equivalent to
  * `LagUD.kr(LagUD.kr(LagUD.kr(in, up, down), up, down), up, down)` , thus
  * resulting in a smoother transition. This saves on CPU as you only have to
  * calculate the decay factors once instead of three times.
  * 
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  * @see [[de.sciss.synth.ugen.Lag3$ Lag3]]
  * @see [[de.sciss.synth.ugen.Lag2UD$ Lag2UD]]
  */
object Lag3UD extends ProductReader[Lag3UD] {
  /** @param in               input signal.
    * @param timeUp           60 dB lag time in seconds effective during a rising
    *                         slope in the input signal
    * @param timeDown         60 dB lag time in seconds effective during a falling
    *                         slope in the input signal
    */
  def kr(in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f): Lag3UD = 
    new Lag3UD(control, in, timeUp, timeDown)
  
  /** @param in               input signal.
    * @param timeUp           60 dB lag time in seconds effective during a rising
    *                         slope in the input signal
    * @param timeDown         60 dB lag time in seconds effective during a falling
    *                         slope in the input signal
    */
  def ar(in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f): Lag3UD = 
    new Lag3UD(audio, in, timeUp, timeDown)
  
  def read(in: RefMapIn, arity: Int): Lag3UD = {
    require (arity == 4)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _timeUp   = in.readGE()
    val _timeDown = in.readGE()
    new Lag3UD(_rate, _in, _timeUp, _timeDown)
  }
}

/** A cascaded exponential lag UGen with separate inputs for up and down slope.
  * `Lag3UD.kr(in, up, down)` is equivalent to
  * `LagUD.kr(LagUD.kr(LagUD.kr(in, up, down), up, down), up, down)` , thus
  * resulting in a smoother transition. This saves on CPU as you only have to
  * calculate the decay factors once instead of three times.
  * 
  * @param in               input signal.
  * @param timeUp           60 dB lag time in seconds effective during a rising
  *                         slope in the input signal
  * @param timeDown         60 dB lag time in seconds effective during a falling
  *                         slope in the input signal
  * 
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  * @see [[de.sciss.synth.ugen.Lag3$ Lag3]]
  * @see [[de.sciss.synth.ugen.Lag2UD$ Lag2UD]]
  */
final case class Lag3UD(rate: MaybeRate, in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, timeUp.expand, timeDown.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A one pole (IIR) filter UGen. Implements the formula :
  * {{{
  * out(i) = ((1 - abs(coef)) * in(i)) + (coef * out(i-1))
  * }}}
  * 
  * 
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  * @see [[de.sciss.synth.ugen.TwoPole$ TwoPole]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
object OnePole extends ProductReader[OnePole] {
  /** @param in               input signal to be processed
    * @param coeff            feedback coefficient. Should be between -1 and +1
    */
  def kr(in: GE, coeff: GE = 0.5f): OnePole = new OnePole(control, in, coeff)
  
  /** @param in               input signal to be processed
    * @param coeff            feedback coefficient. Should be between -1 and +1
    */
  def ar(in: GE, coeff: GE = 0.5f): OnePole = new OnePole(audio, in, coeff)
  
  def read(in: RefMapIn, arity: Int): OnePole = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _coeff  = in.readGE()
    new OnePole(_rate, _in, _coeff)
  }
}

/** A one pole (IIR) filter UGen. Implements the formula :
  * {{{
  * out(i) = ((1 - abs(coef)) * in(i)) + (coef * out(i-1))
  * }}}
  * 
  * 
  * @param in               input signal to be processed
  * @param coeff            feedback coefficient. Should be between -1 and +1
  * 
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  * @see [[de.sciss.synth.ugen.TwoPole$ TwoPole]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
final case class OnePole(rate: MaybeRate, in: GE, coeff: GE = 0.5f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, coeff.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A one zero (FIR) filter UGen. Implements the formula :
  * {{{
  * out(i) = ((1 - abs(coef)) * in(i)) + (coef * in(i-1))
  * }}}
  * 
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.TwoZero$ TwoZero]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  */
object OneZero extends ProductReader[OneZero] {
  /** @param in               input signal to be processed
    * @param coeff            feed forward coefficient. +0.5 makes a two point
    *                         averaging filter (see also `LPZ1` ), -0.5 makes a
    *                         differentiator (see also `HPZ1` ), +1 makes a single
    *                         sample delay (see also `Delay1` ), -1 makes an inverted
    *                         single sample delay.
    */
  def kr(in: GE, coeff: GE = 0.5f): OneZero = new OneZero(control, in, coeff)
  
  /** @param in               input signal to be processed
    * @param coeff            feed forward coefficient. +0.5 makes a two point
    *                         averaging filter (see also `LPZ1` ), -0.5 makes a
    *                         differentiator (see also `HPZ1` ), +1 makes a single
    *                         sample delay (see also `Delay1` ), -1 makes an inverted
    *                         single sample delay.
    */
  def ar(in: GE, coeff: GE = 0.5f): OneZero = new OneZero(audio, in, coeff)
  
  def read(in: RefMapIn, arity: Int): OneZero = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _coeff  = in.readGE()
    new OneZero(_rate, _in, _coeff)
  }
}

/** A one zero (FIR) filter UGen. Implements the formula :
  * {{{
  * out(i) = ((1 - abs(coef)) * in(i)) + (coef * in(i-1))
  * }}}
  * 
  * 
  * @param in               input signal to be processed
  * @param coeff            feed forward coefficient. +0.5 makes a two point
  *                         averaging filter (see also `LPZ1` ), -0.5 makes a
  *                         differentiator (see also `HPZ1` ), +1 makes a single
  *                         sample delay (see also `Delay1` ), -1 makes an inverted
  *                         single sample delay.
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.TwoZero$ TwoZero]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  */
final case class OneZero(rate: MaybeRate, in: GE, coeff: GE = 0.5f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, coeff.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A two pole filter UGen. This provides lower level access to setting of pole
  * location. For general purposes `Resonz` is better.
  * 
  * ===Examples===
  * 
  * {{{
  * // static frequency
  * play { TwoPole.ar(WhiteNoise.ar(0.005), 2000, 0.95) }
  * }}}
  * {{{
  * // sweeping frequency
  * play { TwoPole.ar(WhiteNoise.ar(0.005), XLine.kr(800, 8000, 8), 0.95) }
  * }}}
  * {{{
  * // mouse controlled frequency
  * play { TwoPole.ar(WhiteNoise.ar(0.005), MouseX.kr(800, 8000, 1), 0.95) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.TwoZero$ TwoZero]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
object TwoPole extends ProductReader[TwoPole] {
  /** @param in               input signal to be filtered
    * @param freq             frequency of pole angle, in Hertz
    * @param radius           radius of pole angle. Should be between 0 and 1
    */
  def kr(in: GE, freq: GE = 440.0f, radius: GE = 0.8f): TwoPole = new TwoPole(control, in, freq, radius)
  
  /** @param in               input signal to be filtered
    * @param freq             frequency of pole angle, in Hertz
    * @param radius           radius of pole angle. Should be between 0 and 1
    */
  def ar(in: GE, freq: GE = 440.0f, radius: GE = 0.8f): TwoPole = new TwoPole(audio, in, freq, radius)
  
  def read(in: RefMapIn, arity: Int): TwoPole = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _radius = in.readGE()
    new TwoPole(_rate, _in, _freq, _radius)
  }
}

/** A two pole filter UGen. This provides lower level access to setting of pole
  * location. For general purposes `Resonz` is better.
  * 
  * @param in               input signal to be filtered
  * @param freq             frequency of pole angle, in Hertz
  * @param radius           radius of pole angle. Should be between 0 and 1
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.TwoZero$ TwoZero]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
final case class TwoPole(rate: MaybeRate, in: GE, freq: GE = 440.0f, radius: GE = 0.8f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, radius.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A two zero filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // frequency sweep
  * play { TwoZero.ar(WhiteNoise.ar(0.125), XLine.kr(20, 20000, 8), 1) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  * @see [[de.sciss.synth.ugen.TwoPole$ TwoPole]]
  */
object TwoZero extends ProductReader[TwoZero] {
  /** @param in               input signal to be filtered
    * @param freq             frequency of zero angle, in Hertz
    * @param radius           radius of zero
    */
  def kr(in: GE, freq: GE = 440.0f, radius: GE = 0.8f): TwoZero = new TwoZero(control, in, freq, radius)
  
  /** @param in               input signal to be filtered
    * @param freq             frequency of zero angle, in Hertz
    * @param radius           radius of zero
    */
  def ar(in: GE, freq: GE = 440.0f, radius: GE = 0.8f): TwoZero = new TwoZero(audio, in, freq, radius)
  
  def read(in: RefMapIn, arity: Int): TwoZero = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _radius = in.readGE()
    new TwoZero(_rate, _in, _freq, _radius)
  }
}

/** A two zero filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             frequency of zero angle, in Hertz
  * @param radius           radius of zero
  * 
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  * @see [[de.sciss.synth.ugen.TwoPole$ TwoPole]]
  */
final case class TwoZero(rate: MaybeRate, in: GE, freq: GE = 440.0f, radius: GE = 0.8f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, radius.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** An integrator UGen with exponential decay of past values. This is essentially
  * the same as `Integrator` except that instead of supplying the coefficient
  * directly, it is calculated from a 60 dB decay time. This is the time required
  * for the integrator to lose 99.9 % of its value or -60dB.
  * 
  * Note: This should not be confused with `Lag` which does not overshoot due to
  * integration, but asymptotically follows the input signal.
  * 
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.Decay2$ Decay2]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
object Decay extends ProductReader[Decay] {
  /** @param in               input signal to be processed
    */
  def kr(in: GE, time: GE = 1.0f): Decay = new Decay(control, in, time)
  
  /** @param in               input signal to be processed
    */
  def ar(in: GE, time: GE = 1.0f): Decay = new Decay(audio, in, time)
  
  def read(in: RefMapIn, arity: Int): Decay = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _time = in.readGE()
    new Decay(_rate, _in, _time)
  }
}

/** An integrator UGen with exponential decay of past values. This is essentially
  * the same as `Integrator` except that instead of supplying the coefficient
  * directly, it is calculated from a 60 dB decay time. This is the time required
  * for the integrator to lose 99.9 % of its value or -60dB.
  * 
  * Note: This should not be confused with `Lag` which does not overshoot due to
  * integration, but asymptotically follows the input signal.
  * 
  * @param in               input signal to be processed
  * 
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.Decay2$ Decay2]]
  * @see [[de.sciss.synth.ugen.Lag$ Lag]]
  */
final case class Decay(rate: MaybeRate, in: GE, time: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, time.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A integrator UGen with controllable attack and release times. While `Decay` has
  * a very sharp attack and can produce clicks, `Decay2` rounds off the attack by
  * subtracting one Decay from another. It can be seen as equivalent to
  * {{{
  * Decay.ar(in, release) - Decay.ar(in, attack)
  * }}}
  * 
  * Note: This should not be confused with `LagUD` which does not overshoot due to
  * integration, but asymptotically follows the input signal.
  * 
  * @see [[de.sciss.synth.ugen.Decay$ Decay]]
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  */
object Decay2 extends ProductReader[Decay2] {
  /** @param in               input signal to be processed
    */
  def kr(in: GE, attack: GE = 0.01f, release: GE = 1.0f): Decay2 = 
    new Decay2(control, in, attack, release)
  
  /** @param in               input signal to be processed
    */
  def ar(in: GE, attack: GE = 0.01f, release: GE = 1.0f): Decay2 = new Decay2(audio, in, attack, release)
  
  def read(in: RefMapIn, arity: Int): Decay2 = {
    require (arity == 4)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _attack   = in.readGE()
    val _release  = in.readGE()
    new Decay2(_rate, _in, _attack, _release)
  }
}

/** A integrator UGen with controllable attack and release times. While `Decay` has
  * a very sharp attack and can produce clicks, `Decay2` rounds off the attack by
  * subtracting one Decay from another. It can be seen as equivalent to
  * {{{
  * Decay.ar(in, release) - Decay.ar(in, attack)
  * }}}
  * 
  * Note: This should not be confused with `LagUD` which does not overshoot due to
  * integration, but asymptotically follows the input signal.
  * 
  * @param in               input signal to be processed
  * 
  * @see [[de.sciss.synth.ugen.Decay$ Decay]]
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  */
final case class Decay2(rate: MaybeRate, in: GE, attack: GE = 0.01f, release: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, attack.expand, release.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that delays the input by 1 audio frame or control period.
  * 
  * For audio-rate signals the delay is 1 audio frame, and for control-rate signals
  * the delay is 1 control period.
  * 
  * ''Note:'' The first value output is not zero but the same as the first input
  * value! In this respect the UGen behaves different than `DelayN` .
  * 
  * ===Examples===
  * 
  * {{{
  * // analog to HPZ1
  * play {
  *   val z = PinkNoise.ar
  *   val x = z - Delay1.ar(z)
  *   // mouse button to compare dry/wet
  *   LinXFade2.ar(z, x, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Delay2$ Delay2]]
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  */
object Delay1 extends ProductReader[Delay1] {
  /** @param in               input to be delayed
    */
  def kr(in: GE): Delay1 = new Delay1(control, in)
  
  /** @param in               input to be delayed
    */
  def ar(in: GE): Delay1 = new Delay1(audio, in)
  
  def read(in: RefMapIn, arity: Int): Delay1 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new Delay1(_rate, _in)
  }
}

/** A UGen that delays the input by 1 audio frame or control period.
  * 
  * For audio-rate signals the delay is 1 audio frame, and for control-rate signals
  * the delay is 1 control period.
  * 
  * ''Note:'' The first value output is not zero but the same as the first input
  * value! In this respect the UGen behaves different than `DelayN` .
  * 
  * @param in               input to be delayed
  * 
  * @see [[de.sciss.synth.ugen.Delay2$ Delay2]]
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  */
final case class Delay1(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that delays the input by 2 audio frames or control periods.
  * 
  * For audio-rate signals the delay is 2 audio frames, and for control-rate
  * signals the delay is 2 control periods.
  * 
  * ''Warning:'' the The first value output is zero, while both the second and the
  * third value output equal the first input value! In this respect the UGen behaves
  * different than `DelayN` .
  * 
  * ===Examples===
  * 
  * {{{
  * // high-frequency comb filter
  * play {
  *   val z = PinkNoise.ar
  *   val x = z - Delay2.ar(z)
  *   // mouse button to compare dry/wet
  *   LinXFade2.ar(z, x, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  */
object Delay2 extends ProductReader[Delay2] {
  /** @param in               input to be delayed
    */
  def kr(in: GE): Delay2 = new Delay2(control, in)
  
  /** @param in               input to be delayed
    */
  def ar(in: GE): Delay2 = new Delay2(audio, in)
  
  def read(in: RefMapIn, arity: Int): Delay2 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new Delay2(_rate, _in)
  }
}

/** A UGen that delays the input by 2 audio frames or control periods.
  * 
  * For audio-rate signals the delay is 2 audio frames, and for control-rate
  * signals the delay is 2 control periods.
  * 
  * ''Warning:'' the The first value output is zero, while both the second and the
  * third value output equal the first input value! In this respect the UGen behaves
  * different than `DelayN` .
  * 
  * @param in               input to be delayed
  * 
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  * @see [[de.sciss.synth.ugen.DelayN$ DelayN]]
  */
final case class Delay2(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A filter UGen to integrate an input signal with a leak. Implements the formula :
  * {{{
  * out(i) = in(i) + (coef * out(i-1))
  * }}}
  * 
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  */
object Integrator extends ProductReader[Integrator] {
  /** @param in               input signal to be processed
    * @param coeff            the leak coefficient. Should be between -1 and +1
    */
  def kr(in: GE, coeff: GE = 1.0f): Integrator = new Integrator(control, in, coeff)
  
  /** @param in               input signal to be processed
    * @param coeff            the leak coefficient. Should be between -1 and +1
    */
  def ar(in: GE, coeff: GE = 1.0f): Integrator = new Integrator(audio, in, coeff)
  
  def read(in: RefMapIn, arity: Int): Integrator = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _coeff  = in.readGE()
    new Integrator(_rate, _in, _coeff)
  }
}

/** A filter UGen to integrate an input signal with a leak. Implements the formula :
  * {{{
  * out(i) = in(i) + (coef * out(i-1))
  * }}}
  * 
  * 
  * @param in               input signal to be processed
  * @param coeff            the leak coefficient. Should be between -1 and +1
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  */
final case class Integrator(rate: MaybeRate, in: GE, coeff: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, coeff.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A filter UGen to remove very low frequency content DC offset.
  * 
  * This is a one-pole highpass filter implementing the formula
  * {{{
  * y[n] = x[n] - x[n-1] + coeff * y[n-1]
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // show DC with mouse-controlled coefficient
  * play {
  *   val freq = 800
  *   val in   = LFPulse.ar(freq).mulAdd(0.5, 0.5)
  *   val coef = MouseX.kr(0.9, 0.999)
  *   val flt  = LeakDC.ar(in, coef)
  *   val dc   = RunningSum.ar(flt, SampleRate.ir/freq)
  *   coef.poll(2)
  *   dc  .poll(2)
  *   0
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.DC$ DC]]
  */
object LeakDC extends ProductReader[LeakDC] {
  /** @param in               input signal to be filtered
    * @param coeff            the leak coefficient determines the filter strength.
    *                         the value must be between zero and one (exclusive) for
    *                         the filter to remain stable. values closer to one
    *                         produce less bass attenuation.
    */
  def kr(in: GE, coeff: GE = 0.9f): LeakDC = new LeakDC(control, in, coeff)
  
  /** @param in               input signal to be filtered
    * @param coeff            the leak coefficient determines the filter strength.
    *                         the value must be between zero and one (exclusive) for
    *                         the filter to remain stable. values closer to one
    *                         produce less bass attenuation.
    */
  def ar(in: GE, coeff: GE = 0.995f): LeakDC = new LeakDC(audio, in, coeff)
  
  def read(in: RefMapIn, arity: Int): LeakDC = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _coeff  = in.readGE()
    new LeakDC(_rate, _in, _coeff)
  }
}

/** A filter UGen to remove very low frequency content DC offset.
  * 
  * This is a one-pole highpass filter implementing the formula
  * {{{
  * y[n] = x[n] - x[n-1] + coeff * y[n-1]
  * }}}
  * 
  * 
  * @param in               input signal to be filtered
  * @param coeff            the leak coefficient determines the filter strength.
  *                         the value must be between zero and one (exclusive) for
  *                         the filter to remain stable. values closer to one
  *                         produce less bass attenuation.
  * 
  * @see [[de.sciss.synth.ugen.DC$ DC]]
  */
final case class LeakDC(rate: MaybeRate, in: GE, coeff: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, coeff.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** two point average filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) + in(i-1))
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val sig = WhiteNoise.ar(0.5)
  *   val flt = LPZ1.ar(sig)
  *   LinXFade2.ar(sig, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.LPZ2$ LPZ2]]
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  */
object LPZ1 extends ProductReader[LPZ1] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE): LPZ1 = new LPZ1(control, in)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE): LPZ1 = new LPZ1(audio, in)
  
  def read(in: RefMapIn, arity: Int): LPZ1 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new LPZ1(_rate, _in)
  }
}

/** two point average filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) + in(i-1))
  * }}}
  * 
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.LPZ2$ LPZ2]]
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  */
final case class LPZ1(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A two point difference filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) - in(i-1))
  * }}}
  * 
  * `` Note: `` the filter's memory is initialized with the first input sample, so
  * for `HPZ1.ar(DC.ar(x))` the output will be zero, even at the beginning.
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val sig = PinkNoise.ar
  *   val flt = HPZ1.ar(sig)
  *   LinXFade2.ar(sig, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * {{{
  * // detect changes
  * play {
  *   val z  = LFNoise0.ar(2)
  *   val f  = HPZ1.ar(z)
  *   val ch = f sig_!= 0 // input increased or decreased
  *   z.poll(ch, "now")
  *   0
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  */
object HPZ1 extends ProductReader[HPZ1] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE): HPZ1 = new HPZ1(control, in)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE): HPZ1 = new HPZ1(audio, in)
  
  def read(in: RefMapIn, arity: Int): HPZ1 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new HPZ1(_rate, _in)
  }
}

/** A two point difference filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) - in(i-1))
  * }}}
  * 
  * `` Note: `` the filter's memory is initialized with the first input sample, so
  * for `HPZ1.ar(DC.ar(x))` the output will be zero, even at the beginning.
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.Delay1$ Delay1]]
  */
final case class HPZ1(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** three point average filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.25 * (in(i) + 2 * in(i-1) + in(i-2))
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val sig = WhiteNoise.ar(0.5)
  *   val flt = LPZ2.ar(sig)
  *   LinXFade2.ar(sig, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.BPZ2$ BPZ2]]
  * @see [[de.sciss.synth.ugen.BRZ2$ BRZ2]]
  */
object LPZ2 extends ProductReader[LPZ2] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE): LPZ2 = new LPZ2(control, in)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE): LPZ2 = new LPZ2(audio, in)
  
  def read(in: RefMapIn, arity: Int): LPZ2 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new LPZ2(_rate, _in)
  }
}

/** three point average filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.25 * (in(i) + 2 * in(i-1) + in(i-2))
  * }}}
  * 
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.BPZ2$ BPZ2]]
  * @see [[de.sciss.synth.ugen.BRZ2$ BRZ2]]
  */
final case class LPZ2(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** three point difference filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.25 * (in(i) - 2 * in(i-1) + in(i-2))
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val sig = WhiteNoise.ar(0.5)
  *   val flt = HPZ2.ar(sig)
  *   LinXFade2.ar(sig, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.BPZ2$ BPZ2]]
  * @see [[de.sciss.synth.ugen.BRZ2$ BRZ2]]
  */
object HPZ2 extends ProductReader[HPZ2] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE): HPZ2 = new HPZ2(control, in)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE): HPZ2 = new HPZ2(audio, in)
  
  def read(in: RefMapIn, arity: Int): HPZ2 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new HPZ2(_rate, _in)
  }
}

/** three point difference filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.25 * (in(i) - 2 * in(i-1) + in(i-2))
  * }}}
  * 
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.BPZ2$ BPZ2]]
  * @see [[de.sciss.synth.ugen.BRZ2$ BRZ2]]
  */
final case class HPZ2(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** a special fixed band-pass filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) - in(i-2))
  * }}}
  *  This filter cuts out frequencies around zero Hertz and Nyquist.
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val sig = WhiteNoise.ar(0.5)
  *   val flt = BPZ2.ar(sig)
  *   LinXFade2.ar(sig, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.LPZ2$ LPZ2]]
  * @see [[de.sciss.synth.ugen.BRZ2$ BRZ2]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  */
object BPZ2 extends ProductReader[BPZ2] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE): BPZ2 = new BPZ2(control, in)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE): BPZ2 = new BPZ2(audio, in)
  
  def read(in: RefMapIn, arity: Int): BPZ2 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new BPZ2(_rate, _in)
  }
}

/** a special fixed band-pass filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) - in(i-2))
  * }}}
  *  This filter cuts out frequencies around zero Hertz and Nyquist.
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.LPZ2$ LPZ2]]
  * @see [[de.sciss.synth.ugen.BRZ2$ BRZ2]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  */
final case class BPZ2(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** a special fixed band-reject filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) + in(i-2))
  * }}}
  *  This filter cuts out frequencies around half of the Nyquist frequency.
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val sig = WhiteNoise.ar(0.5)
  *   val flt = BRZ2.ar(sig)
  *   LinXFade2.ar(sig, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  */
object BRZ2 extends ProductReader[BRZ2] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE): BRZ2 = new BRZ2(control, in)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE): BRZ2 = new BRZ2(audio, in)
  
  def read(in: RefMapIn, arity: Int): BRZ2 = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new BRZ2(_rate, _in)
  }
}

/** a special fixed band-reject filter UGen. Implements the formula :
  * {{{
  * out(i) = 0.5 * (in(i) + in(i-2))
  * }}}
  *  This filter cuts out frequencies around half of the Nyquist frequency.
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  * @see [[de.sciss.synth.ugen.HPZ2$ HPZ2]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  */
final case class BRZ2(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}
object APF extends ProductReader[APF] {
  def kr(in: GE, freq: GE = 440.0f, radius: GE = 0.8f): APF = new APF(control, in, freq, radius)
  
  def ar(in: GE, freq: GE = 440.0f, radius: GE = 0.8f): APF = new APF(audio, in, freq, radius)
  
  def read(in: RefMapIn, arity: Int): APF = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _radius = in.readGE()
    new APF(_rate, _in, _freq, _radius)
  }
}
final case class APF(rate: MaybeRate, in: GE, freq: GE = 440.0f, radius: GE = 0.8f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, radius.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A second order low pass filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulated frequency
  * play {
  *   val in   = Saw.ar(200) * 0.5
  *   val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).mulAdd(3600, 4000)
  *   LPF.ar(in, freq)
  * }
  * }}}
  * {{{
  * // mouse controlled frequency
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   LPF.ar(in, freq)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  */
object LPF extends ProductReader[LPF] {
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    */
  def kr(in: GE, freq: GE = 440.0f): LPF = new LPF(control, in, freq)
  
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    */
  def ar(in: GE, freq: GE = 440.0f): LPF = new LPF(audio, in, freq)
  
  def read(in: RefMapIn, arity: Int): LPF = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    new LPF(_rate, _in, _freq)
  }
}

/** A second order low pass filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             cutoff frequency in Hertz
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.LPZ1$ LPZ1]]
  */
final case class LPF(rate: MaybeRate, in: GE, freq: GE = 440.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A second order high pass filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulated frequency
  * play {
  *   val in   = Saw.ar(200) * 0.5
  *   val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).mulAdd(3600, 4000)
  *   HPF.ar(in, freq)
  * }
  * }}}
  * {{{
  * // mouse controlled frequency
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   HPF.ar(in, freq)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  */
object HPF extends ProductReader[HPF] {
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    */
  def kr(in: GE, freq: GE = 440.0f): HPF = new HPF(control, in, freq)
  
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    */
  def ar(in: GE, freq: GE = 440.0f): HPF = new HPF(audio, in, freq)
  
  def read(in: RefMapIn, arity: Int): HPF = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    new HPF(_rate, _in, _freq)
  }
}

/** A second order high pass filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             cutoff frequency in Hertz
  * 
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  * @see [[de.sciss.synth.ugen.HPZ1$ HPZ1]]
  */
final case class HPF(rate: MaybeRate, in: GE, freq: GE = 440.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A second order band pass filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulated frequency
  * play {
  *   val in   = Saw.ar(200) * 0.5
  *   val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).mulAdd(3600, 4000)
  *   BPF.ar(in, freq)
  * }
  * }}}
  * {{{
  * // mouse controlled frequency and Q
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   val q    = MouseY.kr(1, 100, 1) // bottom to top
  *   val flt  = BPF.ar(in, freq, q.reciprocal)
  *   flt * q.sqrt // compensate for energy loss
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
object BPF extends ProductReader[BPF] {
  /** @param in               input signal to be filtered
    * @param freq             center frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def kr(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): BPF = new BPF(control, in, freq, rq)
  
  /** @param in               input signal to be filtered
    * @param freq             center frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def ar(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): BPF = new BPF(audio, in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): BPF = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new BPF(_rate, _in, _freq, _rq)
  }
}

/** A second order band pass filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             center frequency in Hertz
  * @param rq               reciprocal of Q. The Q (or quality) is conventionally
  *                         defined as center-frequency / bandwidth, meaning that rq
  *                         &nbsp;= bandwidth / center-frequency. A higher Q or
  *                         lower rq produces a steeper filter.
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
final case class BPF(rate: MaybeRate, in: GE, freq: GE = 440.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A second order band reject (notch) filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulated frequency
  * play {
  *   val in   = Saw.ar(200) * 0.5
  *   val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).mulAdd(3600, 4000)
  *   BRF.ar(in, freq)
  * }
  * }}}
  * {{{
  * // mouse controlled frequency and Q
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   val q    = MouseY.kr(0.5, 10, 1) // bottom to top
  *   BRF.ar(in, freq, q.reciprocal)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
object BRF extends ProductReader[BRF] {
  /** @param in               input signal to be filtered
    * @param freq             center frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter. Too high values for
    *                         `rq` may blow the filter up!
    */
  def kr(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): BRF = new BRF(control, in, freq, rq)
  
  /** @param in               input signal to be filtered
    * @param freq             center frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter. Too high values for
    *                         `rq` may blow the filter up!
    */
  def ar(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): BRF = new BRF(audio, in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): BRF = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new BRF(_rate, _in, _freq, _rq)
  }
}

/** A second order band reject (notch) filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             center frequency in Hertz
  * @param rq               reciprocal of Q. The Q (or quality) is conventionally
  *                         defined as center-frequency / bandwidth, meaning that rq
  *                         &nbsp;= bandwidth / center-frequency. A higher Q or
  *                         lower rq produces a steeper filter. Too high values for
  *                         `rq` may blow the filter up!
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
final case class BRF(rate: MaybeRate, in: GE, freq: GE = 440.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A resonant low pass filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse controlled frequency and Q
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   val q    = MouseY.kr(1, 100, 1) // bottom to top
  *   val flt  = RLPF.ar(in, freq, q.reciprocal)
  *   flt / q.sqrt // compensate for energy loss
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
object RLPF extends ProductReader[RLPF] {
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as cutoff-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / cutoff-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def kr(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): RLPF = new RLPF(control, in, freq, rq)
  
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as cutoff-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / cutoff-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def ar(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): RLPF = new RLPF(audio, in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): RLPF = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new RLPF(_rate, _in, _freq, _rq)
  }
}

/** A resonant low pass filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             cutoff frequency in Hertz
  * @param rq               reciprocal of Q. The Q (or quality) is conventionally
  *                         defined as cutoff-frequency / bandwidth, meaning that rq
  *                         &nbsp;= bandwidth / cutoff-frequency. A higher Q or
  *                         lower rq produces a steeper filter.
  * 
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
final case class RLPF(rate: MaybeRate, in: GE, freq: GE = 440.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A resonant high pass filter UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse controlled frequency and Q
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   val q    = MouseY.kr(1, 100, 1) // bottom to top
  *   val flt  = RHPF.ar(in, freq, q.reciprocal)
  *   flt / q.sqrt // compensate for energy loss
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
object RHPF extends ProductReader[RHPF] {
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as cutoff-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / cutoff-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def kr(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): RHPF = new RHPF(control, in, freq, rq)
  
  /** @param in               input signal to be filtered
    * @param freq             cutoff frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as cutoff-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / cutoff-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def ar(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): RHPF = new RHPF(audio, in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): RHPF = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new RHPF(_rate, _in, _freq, _rq)
  }
}

/** A resonant high pass filter UGen.
  * 
  * @param in               input signal to be filtered
  * @param freq             cutoff frequency in Hertz
  * @param rq               reciprocal of Q. The Q (or quality) is conventionally
  *                         defined as cutoff-frequency / bandwidth, meaning that rq
  *                         &nbsp;= bandwidth / cutoff-frequency. A higher Q or
  *                         lower rq produces a steeper filter.
  * 
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
final case class RHPF(rate: MaybeRate, in: GE, freq: GE = 440.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A slew rate limiter UGen. Limits the slope of an input signal. The slope is
  * expressed in units per second.
  * 
  * Since the UGen is initialized with the initial value of the input signal, some
  * tricks must be applied to set it to an alternative start value. For example:
  * {{{
  * val in = Select.kr(ToggleFF.kr(1), Seq("start".ir, "target".kr))
  * Slew.kr(in)  // begins at "start" and moves towards "target"
  * }}}
  * 
  */
object Slew extends ProductReader[Slew] {
  /** @param in               input signal
    * @param up               maximum upward slope.
    * @param down             maximum downward slope.
    */
  def kr(in: GE, up: GE = 1.0f, down: GE = 1.0f): Slew = new Slew(control, in, up, down)
  
  /** @param in               input signal
    * @param up               maximum upward slope.
    * @param down             maximum downward slope.
    */
  def ar(in: GE, up: GE = 1.0f, down: GE = 1.0f): Slew = new Slew(audio, in, up, down)
  
  def read(in: RefMapIn, arity: Int): Slew = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _up   = in.readGE()
    val _down = in.readGE()
    new Slew(_rate, _in, _up, _down)
  }
}

/** A slew rate limiter UGen. Limits the slope of an input signal. The slope is
  * expressed in units per second.
  * 
  * Since the UGen is initialized with the initial value of the input signal, some
  * tricks must be applied to set it to an alternative start value. For example:
  * {{{
  * val in = Select.kr(ToggleFF.kr(1), Seq("start".ir, "target".kr))
  * Slew.kr(in)  // begins at "start" and moves towards "target"
  * }}}
  * 
  * 
  * @param in               input signal
  * @param up               maximum upward slope.
  * @param down             maximum downward slope.
  */
final case class Slew(rate: MaybeRate, in: GE, up: GE = 1.0f, down: GE = 1.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, up.expand, down.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen measuring the slope of signal. It calculates the rate of change per
  * second of a signal, as given by the following formula:
  * {{{
  * out(i) = (in(i) - in(i-1)) * sampleRate
  * }}}
  *  It thus equal to `HPZ1.ar(_) * 2 * SampleRate.ir`
  */
object Slope extends ProductReader[Slope] {
  /** @param in               input signal to be measured
    */
  def kr(in: GE): Slope = new Slope(control, in)
  
  /** @param in               input signal to be measured
    */
  def ar(in: GE): Slope = new Slope(audio, in)
  
  def read(in: RefMapIn, arity: Int): Slope = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new Slope(_rate, _in)
  }
}

/** A UGen measuring the slope of signal. It calculates the rate of change per
  * second of a signal, as given by the following formula:
  * {{{
  * out(i) = (in(i) - in(i-1)) * sampleRate
  * }}}
  *  It thus equal to `HPZ1.ar(_) * 2 * SampleRate.ir`
  * 
  * @param in               input signal to be measured
  */
final case class Slope(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A single band parametric equalizer UGen. It attenuates or boosts a frequency
  * band.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse controlled frequency and boost
  * play {
  *   val in   = WhiteNoise.ar(0.25)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   val gain = MouseY.kr(-12, 12) // bottom to top
  *   MidEQ.ar(in, freq, rq = 0.5, gain = gain)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
object MidEQ extends ProductReader[MidEQ] {
  /** @param in               input signal to be filtered
    * @param freq             center frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter. Too high values for
    *                         `rq` may blow the filter up!
    * @param gain             The amount of boost (when positive) or attenuation
    *                         (when negative) applied to the frequency band, in
    *                         decibels.
    */
  def kr(in: GE, freq: GE = 440.0f, rq: GE = 1.0f, gain: GE = 0.0f): MidEQ = 
    new MidEQ(control, in, freq, rq, gain)
  
  /** @param in               input signal to be filtered
    * @param freq             center frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter. Too high values for
    *                         `rq` may blow the filter up!
    * @param gain             The amount of boost (when positive) or attenuation
    *                         (when negative) applied to the frequency band, in
    *                         decibels.
    */
  def ar(in: GE, freq: GE = 440.0f, rq: GE = 1.0f, gain: GE = 0.0f): MidEQ = 
    new MidEQ(audio, in, freq, rq, gain)
  
  def read(in: RefMapIn, arity: Int): MidEQ = {
    require (arity == 5)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    val _gain = in.readGE()
    new MidEQ(_rate, _in, _freq, _rq, _gain)
  }
}

/** A single band parametric equalizer UGen. It attenuates or boosts a frequency
  * band.
  * 
  * @param in               input signal to be filtered
  * @param freq             center frequency in Hertz
  * @param rq               reciprocal of Q. The Q (or quality) is conventionally
  *                         defined as center-frequency / bandwidth, meaning that rq
  *                         &nbsp;= bandwidth / center-frequency. A higher Q or
  *                         lower rq produces a steeper filter. Too high values for
  *                         `rq` may blow the filter up!
  * @param gain             The amount of boost (when positive) or attenuation
  *                         (when negative) applied to the frequency band, in
  *                         decibels.
  * 
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.BRF$ BRF]]
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  */
final case class MidEQ(rate: MaybeRate, in: GE, freq: GE = 440.0f, rq: GE = 1.0f, gain: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand, gain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A filter UGen that calculates the median of a running window over its input
  * signal. This non-linear filter can be used to reduce impulse noise from a
  * signal.
  * 
  * ===Examples===
  * 
  * {{{
  * // engage with mouse button
  * play {
  *   val in  = Saw.ar(500) * 0.1 + Dust2.ar(100) * 0.9 // signal plus noise
  *   val flt = Median.ar(in, 3)
  *   LinXFade2.ar(in, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * {{{
  * // long filter distorts by chopping off peaks in input
  * play { Median.ar(SinOsc.ar(1000) * 0.2, 31) }
  * }}}
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.LeakDC$ LeakDC]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  */
object Median extends ProductReader[Median] {
  /** @param length           window size. I.e., the number of input samples in which
    *                         to find the median. Must be an odd number from 1 to 31.
    *                         A value of 1 has no effect. ''Warning'': This parameter
    *                         is only read an initialization time and cannot be
    *                         modulated while the UGen is running. ''(init-time
    *                         only)''
    */
  def kr(in: GE, length: GE = 3): Median = new Median(control, in, length)
  
  /** @param length           window size. I.e., the number of input samples in which
    *                         to find the median. Must be an odd number from 1 to 31.
    *                         A value of 1 has no effect. ''Warning'': This parameter
    *                         is only read an initialization time and cannot be
    *                         modulated while the UGen is running. ''(init-time
    *                         only)''
    */
  def ar(in: GE, length: GE = 3): Median = new Median(audio, in, length)
  
  def read(in: RefMapIn, arity: Int): Median = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _length = in.readGE()
    new Median(_rate, _in, _length)
  }
}

/** A filter UGen that calculates the median of a running window over its input
  * signal. This non-linear filter can be used to reduce impulse noise from a
  * signal.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param length           window size. I.e., the number of input samples in which
  *                         to find the median. Must be an odd number from 1 to 31.
  *                         A value of 1 has no effect. ''Warning'': This parameter
  *                         is only read an initialization time and cannot be
  *                         modulated while the UGen is running. ''(init-time
  *                         only)''
  * 
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.LeakDC$ LeakDC]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  */
final case class Median(rate: Rate, in: GE, length: GE = 3) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A two pole resonant filter UGen. It has zeroes at `z = +1` and `z = -1`.
  * 
  * Based on K. Steiglitz, "A Note on Constant-Gain Digital Resonators", Computer
  * Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulated frequency
  * play {
  *   val in   = Saw.ar(200) * 0.5
  *   val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).mulAdd(3600, 4000)
  *   Resonz.ar(in, freq)
  * }
  * }}}
  * {{{
  * // mouse controlled frequency and Q
  * play {
  *   val in   = WhiteNoise.ar(0.5)
  *   val freq = MouseX.kr(200, 10000, 1)
  *   val q    = MouseY.kr(1, 100, 1) // bottom to top
  *   val flt  = Resonz.ar(in, freq, q.reciprocal)
  *   flt * q.sqrt // compensate for energy loss
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
object Resonz extends ProductReader[Resonz] {
  /** @param in               input signal to be filtered
    * @param freq             resonant frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def kr(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): Resonz = new Resonz(control, in, freq, rq)
  
  /** @param in               input signal to be filtered
    * @param freq             resonant frequency in Hertz
    * @param rq               reciprocal of Q. The Q (or quality) is conventionally
    *                         defined as center-frequency / bandwidth, meaning that rq
    *                         &nbsp;= bandwidth / center-frequency. A higher Q or
    *                         lower rq produces a steeper filter.
    */
  def ar(in: GE, freq: GE = 440.0f, rq: GE = 1.0f): Resonz = new Resonz(audio, in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): Resonz = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new Resonz(_rate, _in, _freq, _rq)
  }
}

/** A two pole resonant filter UGen. It has zeroes at `z = +1` and `z = -1`.
  * 
  * Based on K. Steiglitz, "A Note on Constant-Gain Digital Resonators", Computer
  * Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994.
  * 
  * @param in               input signal to be filtered
  * @param freq             resonant frequency in Hertz
  * @param rq               reciprocal of Q. The Q (or quality) is conventionally
  *                         defined as center-frequency / bandwidth, meaning that rq
  *                         &nbsp;= bandwidth / center-frequency. A higher Q or
  *                         lower rq produces a steeper filter.
  * 
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  * @see [[de.sciss.synth.ugen.HPF$ HPF]]
  * @see [[de.sciss.synth.ugen.LPF$ LPF]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
final case class Resonz(rate: MaybeRate, in: GE, freq: GE = 440.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A resonant or "ringing" filter UGen. This is the same as `Resonz` , except that
  * instead of a Q parameter, the bandwidth is specified as a 60 dB ring decay time.
  * One `Ringz` is equivalent to one component of the `Klank` UGen.
  * 
  * ''Note'': `Ringz` and derived UGens `Klank` and `Formlet` produce output RMS
  * depending on the server's sampling rate. This is to achieve the same amplitude
  * for single-sample impulse inputs.
  * 
  * ===Examples===
  * 
  * {{{
  * // module ring time
  * play { Ringz.ar(Impulse.ar(6) * 0.3, 2000, XLine.kr(4, 0.04, 8)) }
  * }}}
  * {{{
  * // modulated frequency
  * play {
  *   val in   = Saw.ar(200) * 0.02
  *   val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).mulAdd(2800, 4800)
  *   Ringz.ar(in, freq)
  * }
  * }}}
  * {{{
  * // multiple glissandi excited by noise
  * play {
  *   val ex = WhiteNoise.ar(0.001)
  *   Mix.fill(10) {
  *     Ringz.ar(ex,
  *       XLine.kr(ExpRand(100, 5000), ExpRand(100, 5000), 20),
  *     0.5)
  *   }
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.Formlet$ Formlet]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.Klank$ Klank]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
object Ringz extends ProductReader[Ringz] {
  /** @param in               input signal to be filtered
    * @param freq             resonant frequency in Hertz
    * @param decay            the 60 dB decay time in seconds
    */
  def kr(in: GE, freq: GE = 440.0f, decay: GE = 1.0f): Ringz = new Ringz(control, in, freq, decay)
  
  /** @param in               input signal to be filtered
    * @param freq             resonant frequency in Hertz
    * @param decay            the 60 dB decay time in seconds
    */
  def ar(in: GE, freq: GE = 440.0f, decay: GE = 1.0f): Ringz = new Ringz(audio, in, freq, decay)
  
  def read(in: RefMapIn, arity: Int): Ringz = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _decay  = in.readGE()
    new Ringz(_rate, _in, _freq, _decay)
  }
}

/** A resonant or "ringing" filter UGen. This is the same as `Resonz` , except that
  * instead of a Q parameter, the bandwidth is specified as a 60 dB ring decay time.
  * One `Ringz` is equivalent to one component of the `Klank` UGen.
  * 
  * ''Note'': `Ringz` and derived UGens `Klank` and `Formlet` produce output RMS
  * depending on the server's sampling rate. This is to achieve the same amplitude
  * for single-sample impulse inputs.
  * 
  * @param in               input signal to be filtered
  * @param freq             resonant frequency in Hertz
  * @param decay            the 60 dB decay time in seconds
  * 
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.Formlet$ Formlet]]
  * @see [[de.sciss.synth.ugen.BPF$ BPF]]
  * @see [[de.sciss.synth.ugen.Klank$ Klank]]
  * @see [[de.sciss.synth.ugen.MidEQ$ MidEQ]]
  */
final case class Ringz(rate: MaybeRate, in: GE, freq: GE = 440.0f, decay: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, decay.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A FOF-like resonant filter UGen. Its impulse response is like that of a sine
  * wave with a `Decay2` envelope over it. It is possible to control the attack and
  * decay times.
  * 
  * `Formlet` is equivalent to:
  * {{{
  * Ringz(in, freq, decay) - Ringz(in, freq, attack)
  * }}}
  * 
  * The great advantage to this filter over FOF (Fonction d'onde formantique) is
  * that there is no limit to the number of overlapping grains since the grain is
  * just the impulse response of the filter.
  * 
  * ''Note'': `Ringz` and derived UGens `Klank` and `Formlet` produce output RMS
  * depending on the server's sampling rate. This is to achieve the same amplitude
  * for single-sample impulse inputs.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulated formant frequency
  * play {
  *   val in = Blip.ar(SinOsc.kr(5,0).mulAdd(20, 300), 1000) * 0.1
  *   Formlet.ar(in, XLine.kr(1500, 700, 8), 0.005, 0.04)
  * }
  * }}}
  * {{{
  * // mouse control of frequency and decay time
  * play {
  *   val in    = Blip.ar(SinOsc.kr(5,0).mulAdd(20, 300), 1000) * 0.1
  *   val freq  = MouseY.kr(700, 2000, 1)
  *   val decay = MouseX.kr(0.01, 0.2, 1)
  *   Formlet.ar(in, freq, attack = 0.005, decay = decay)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  */
object Formlet extends ProductReader[Formlet] {
  /** @param in               input signal to be filtered
    * @param freq             resonant frequency in Hertz
    * @param attack           the 60 dB attack time in seconds
    * @param decay            the 60 dB decay time in seconds
    */
  def kr(in: GE, freq: GE = 440.0f, attack: GE = 1.0f, decay: GE = 1.0f): Formlet = 
    new Formlet(control, in, freq, attack, decay)
  
  /** @param in               input signal to be filtered
    * @param freq             resonant frequency in Hertz
    * @param attack           the 60 dB attack time in seconds
    * @param decay            the 60 dB decay time in seconds
    */
  def ar(in: GE, freq: GE = 440.0f, attack: GE = 1.0f, decay: GE = 1.0f): Formlet = 
    new Formlet(audio, in, freq, attack, decay)
  
  def read(in: RefMapIn, arity: Int): Formlet = {
    require (arity == 5)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _attack = in.readGE()
    val _decay  = in.readGE()
    new Formlet(_rate, _in, _freq, _attack, _decay)
  }
}

/** A FOF-like resonant filter UGen. Its impulse response is like that of a sine
  * wave with a `Decay2` envelope over it. It is possible to control the attack and
  * decay times.
  * 
  * `Formlet` is equivalent to:
  * {{{
  * Ringz(in, freq, decay) - Ringz(in, freq, attack)
  * }}}
  * 
  * The great advantage to this filter over FOF (Fonction d'onde formantique) is
  * that there is no limit to the number of overlapping grains since the grain is
  * just the impulse response of the filter.
  * 
  * ''Note'': `Ringz` and derived UGens `Klank` and `Formlet` produce output RMS
  * depending on the server's sampling rate. This is to achieve the same amplitude
  * for single-sample impulse inputs.
  * 
  * @param in               input signal to be filtered
  * @param freq             resonant frequency in Hertz
  * @param attack           the 60 dB attack time in seconds
  * @param decay            the 60 dB decay time in seconds
  * 
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  * @see [[de.sciss.synth.ugen.Resonz$ Resonz]]
  * @see [[de.sciss.synth.ugen.RLPF$ RLPF]]
  * @see [[de.sciss.synth.ugen.RHPF$ RHPF]]
  */
final case class Formlet(rate: MaybeRate, in: GE, freq: GE = 440.0f, attack: GE = 1.0f, decay: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, attack.expand, decay.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A first order filter section UGen. Filter coefficients are given directly
  * rather than calculated for you. The formula is equivalent to:
  * {{{
  * out(i) = a0 * in(i) + a1 * in(i-1) + b1 * out(i-1)
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // same as OnePole
  * play {
  *   val x = LFTri.ar(0.4) * 0.99
  *   FOS.ar(LFSaw.ar(200) * 0.1, 1 - x.abs, 0.0, x)
  * }
  * }}}
  * {{{
  * // same as OneZero
  * play {
  *   val x = LFTri.ar(0.4) * 0.99
  *   FOS.ar(LFSaw.ar(200) * 0.1, 1 - x.abs, x, 0.0)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SOS$ SOS]]
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  */
object FOS extends ProductReader[FOS] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE, a0: GE = 0.0f, a1: GE = 0.0f, b1: GE = 0.0f): FOS = new FOS(control, in, a0, a1, b1)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE, a0: GE = 0.0f, a1: GE = 0.0f, b1: GE = 0.0f): FOS = new FOS(audio, in, a0, a1, b1)
  
  def read(in: RefMapIn, arity: Int): FOS = {
    require (arity == 5)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _a0   = in.readGE()
    val _a1   = in.readGE()
    val _b1   = in.readGE()
    new FOS(_rate, _in, _a0, _a1, _b1)
  }
}

/** A first order filter section UGen. Filter coefficients are given directly
  * rather than calculated for you. The formula is equivalent to:
  * {{{
  * out(i) = a0 * in(i) + a1 * in(i-1) + b1 * out(i-1)
  * }}}
  * 
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.SOS$ SOS]]
  * @see [[de.sciss.synth.ugen.OnePole$ OnePole]]
  * @see [[de.sciss.synth.ugen.OneZero$ OneZero]]
  */
final case class FOS(rate: MaybeRate, in: GE, a0: GE = 0.0f, a1: GE = 0.0f, b1: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, a0.expand, a1.expand, b1.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A second order filter section (biquad) UGen. Filter coefficients are given
  * directly rather than calculated for you. The formula is equivalent to:
  * {{{
  * out(i) = a0 * in(i) + a1 * in(i-1) + a2 * in(i-2) + b1 * out(i-1) + b2 * out(i-2)
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // same as TwoPole
  * play {
  *   val theta = MouseX.kr(0.2*math.Pi, 0.9*math.Pi)
  *   val rho   = MouseY.kr(0.6, 0.98)
  *   val b1    = 2.0 * rho * theta.cos
  *   val b2    = -(rho.squared)
  *   SOS.ar(WhiteNoise.ar(Seq(0.05, 0.05)), 1.0, 0.0, 0.0, b1, b2)
  * }
  * }}}
  * {{{
  * // used as control signal
  * play {
  *   val theta = MouseX.kr(0.2*math.Pi, math.Pi)
  *   val rho   = MouseY.kr(0.6, 0.99)
  *   val b1    = 2.0 * rho * theta.cos
  *   val b2    = -(rho.squared)
  *   val vib   = SOS.kr(LFSaw.kr(3.16), 1.0, 0.0, 0.0, b1, b2)
  *   SinOsc.ar(vib * 200 + 600) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FOS$ FOS]]
  */
object SOS extends ProductReader[SOS] {
  /** @param in               input signal to be filtered
    */
  def kr(in: GE, a0: GE = 0.0f, a1: GE = 0.0f, a2: GE = 0.0f, b1: GE = 0.0f, b2: GE = 0.0f): SOS = 
    new SOS(control, in, a0, a1, a2, b1, b2)
  
  /** @param in               input signal to be filtered
    */
  def ar(in: GE, a0: GE = 0.0f, a1: GE = 0.0f, a2: GE = 0.0f, b1: GE = 0.0f, b2: GE = 0.0f): SOS = 
    new SOS(audio, in, a0, a1, a2, b1, b2)
  
  def read(in: RefMapIn, arity: Int): SOS = {
    require (arity == 7)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _a0   = in.readGE()
    val _a1   = in.readGE()
    val _a2   = in.readGE()
    val _b1   = in.readGE()
    val _b2   = in.readGE()
    new SOS(_rate, _in, _a0, _a1, _a2, _b1, _b2)
  }
}

/** A second order filter section (biquad) UGen. Filter coefficients are given
  * directly rather than calculated for you. The formula is equivalent to:
  * {{{
  * out(i) = a0 * in(i) + a1 * in(i-1) + a2 * in(i-2) + b1 * out(i-1) + b2 * out(i-2)
  * }}}
  * 
  * 
  * @param in               input signal to be filtered
  * 
  * @see [[de.sciss.synth.ugen.FOS$ FOS]]
  */
final case class SOS(rate: MaybeRate, in: GE, a0: GE = 0.0f, a1: GE = 0.0f, a2: GE = 0.0f, b1: GE = 0.0f, b2: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, a0.expand, a1.expand, a2.expand, b1.expand, b2.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A compressor, expander, limiter, gate and ducking UGen. This dynamic processor
  * uses a hard-knee characteristic. All of the thresholds and ratios are given as
  * direct values, not in decibels!
  * 
  * @see [[de.sciss.synth.ugen.Limiter$ Limiter]]
  * @see [[de.sciss.synth.ugen.Normalizer$ Normalizer]]
  */
object Compander extends ProductReader[Compander] {
  /** @param in               The signal to be compressed / expanded / gated.
    * @param ctrl             The signal whose amplitude controls the processor.
    *                         Often the same as in, but one may wish to apply
    *                         equalization or delay to it to change the compressor
    *                         character (side-chaining), or even feed a completely
    *                         different signal, for instance in a ducking application.
    * @param thresh           Control signal amplitude threshold, which determines
    *                         the break point between slopeBelow and slopeAbove.
    *                         Usually 0..1. The control signal amplitude is calculated
    *                         using RMS.
    * @param ratioBelow       Slope of the amplitude curve below the threshold. If
    *                         this slope > 1.0, the amplitude will drop off more
    *                         quickly the softer the control signal gets; when the
    *                         control signal is close to 0 amplitude, the output
    *                         should be exactly zero -- hence, noise gating. Values <
    *                         1.0 are possible, but it means that a very low-level
    *                         control signal will cause the input signal to be
    *                         amplified, which would raise the noise floor.
    * @param ratioAbove       Slope of the amplitude curve above the threshold.
    *                         Values < 1.0 achieve compression (louder signals are
    *                         attenuated); > 1.0, you get expansion (louder signals
    *                         are made even louder). For 3:1 compression, you would
    *                         use a value of 1/3 here.
    * @param attack           The amount of time it takes for the amplitude
    *                         adjustment to kick in fully. This is usually pretty
    *                         small, not much more than 10 milliseconds (the default
    *                         value). I often set it as low as 2 milliseconds (0.002).
    * @param release          The amount of time for the amplitude adjustment to be
    *                         released. Usually a bit longer than attack; if both
    *                         times are too short, you can get some (possibly
    *                         unwanted) artifacts.
    */
  def kr(in: GE, ctrl: GE, thresh: GE = 0.5f, ratioBelow: GE = 1.0f, ratioAbove: GE = 1.0f, attack: GE = 0.01f, release: GE = 0.1f): Compander = 
    new Compander(control, in, ctrl, thresh, ratioBelow, ratioAbove, attack, release)
  
  /** @param in               The signal to be compressed / expanded / gated.
    * @param ctrl             The signal whose amplitude controls the processor.
    *                         Often the same as in, but one may wish to apply
    *                         equalization or delay to it to change the compressor
    *                         character (side-chaining), or even feed a completely
    *                         different signal, for instance in a ducking application.
    * @param thresh           Control signal amplitude threshold, which determines
    *                         the break point between slopeBelow and slopeAbove.
    *                         Usually 0..1. The control signal amplitude is calculated
    *                         using RMS.
    * @param ratioBelow       Slope of the amplitude curve below the threshold. If
    *                         this slope > 1.0, the amplitude will drop off more
    *                         quickly the softer the control signal gets; when the
    *                         control signal is close to 0 amplitude, the output
    *                         should be exactly zero -- hence, noise gating. Values <
    *                         1.0 are possible, but it means that a very low-level
    *                         control signal will cause the input signal to be
    *                         amplified, which would raise the noise floor.
    * @param ratioAbove       Slope of the amplitude curve above the threshold.
    *                         Values < 1.0 achieve compression (louder signals are
    *                         attenuated); > 1.0, you get expansion (louder signals
    *                         are made even louder). For 3:1 compression, you would
    *                         use a value of 1/3 here.
    * @param attack           The amount of time it takes for the amplitude
    *                         adjustment to kick in fully. This is usually pretty
    *                         small, not much more than 10 milliseconds (the default
    *                         value). I often set it as low as 2 milliseconds (0.002).
    * @param release          The amount of time for the amplitude adjustment to be
    *                         released. Usually a bit longer than attack; if both
    *                         times are too short, you can get some (possibly
    *                         unwanted) artifacts.
    */
  def ar(in: GE, ctrl: GE, thresh: GE = 0.5f, ratioBelow: GE = 1.0f, ratioAbove: GE = 1.0f, attack: GE = 0.01f, release: GE = 0.1f): Compander = 
    new Compander(audio, in, ctrl, thresh, ratioBelow, ratioAbove, attack, release)
  
  def read(in: RefMapIn, arity: Int): Compander = {
    require (arity == 8)
    val _rate       = in.readRate()
    val _in         = in.readGE()
    val _ctrl       = in.readGE()
    val _thresh     = in.readGE()
    val _ratioBelow = in.readGE()
    val _ratioAbove = in.readGE()
    val _attack     = in.readGE()
    val _release    = in.readGE()
    new Compander(_rate, _in, _ctrl, _thresh, _ratioBelow, _ratioAbove, _attack, _release)
  }
}

/** A compressor, expander, limiter, gate and ducking UGen. This dynamic processor
  * uses a hard-knee characteristic. All of the thresholds and ratios are given as
  * direct values, not in decibels!
  * 
  * @param in               The signal to be compressed / expanded / gated.
  * @param ctrl             The signal whose amplitude controls the processor.
  *                         Often the same as in, but one may wish to apply
  *                         equalization or delay to it to change the compressor
  *                         character (side-chaining), or even feed a completely
  *                         different signal, for instance in a ducking application.
  * @param thresh           Control signal amplitude threshold, which determines
  *                         the break point between slopeBelow and slopeAbove.
  *                         Usually 0..1. The control signal amplitude is calculated
  *                         using RMS.
  * @param ratioBelow       Slope of the amplitude curve below the threshold. If
  *                         this slope > 1.0, the amplitude will drop off more
  *                         quickly the softer the control signal gets; when the
  *                         control signal is close to 0 amplitude, the output
  *                         should be exactly zero -- hence, noise gating. Values <
  *                         1.0 are possible, but it means that a very low-level
  *                         control signal will cause the input signal to be
  *                         amplified, which would raise the noise floor.
  * @param ratioAbove       Slope of the amplitude curve above the threshold.
  *                         Values < 1.0 achieve compression (louder signals are
  *                         attenuated); > 1.0, you get expansion (louder signals
  *                         are made even louder). For 3:1 compression, you would
  *                         use a value of 1/3 here.
  * @param attack           The amount of time it takes for the amplitude
  *                         adjustment to kick in fully. This is usually pretty
  *                         small, not much more than 10 milliseconds (the default
  *                         value). I often set it as low as 2 milliseconds (0.002).
  * @param release          The amount of time for the amplitude adjustment to be
  *                         released. Usually a bit longer than attack; if both
  *                         times are too short, you can get some (possibly
  *                         unwanted) artifacts.
  * 
  * @see [[de.sciss.synth.ugen.Limiter$ Limiter]]
  * @see [[de.sciss.synth.ugen.Normalizer$ Normalizer]]
  */
final case class Compander(rate: Rate, in: GE, ctrl: GE, thresh: GE = 0.5f, ratioBelow: GE = 1.0f, ratioAbove: GE = 1.0f, attack: GE = 0.01f, release: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, ctrl.expand, thresh.expand, ratioBelow.expand, ratioAbove.expand, attack.expand, release.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Limits the input amplitude to the given level. Unlike `Compander` , this UGen
  * will never overshoot, but it needs to look ahead in the input signal,
  * introducing a delay in its output. The delay time is equal to twice the value of
  * the `dur` parameter (the buffer internally used).
  * 
  * ===Examples===
  * 
  * {{{
  * // compare dry and wet
  * play {
  *   val in = Decay2.ar(
  *     Impulse.ar(8, phase = LFSaw.kr(0.25) * 0.7),
  *     attack = 0.001, release = 0.3) * FSinOsc.ar(500)
  *   val flt = Limiter.ar(in, level = 0.4)
  *   LinXFade2.ar(in, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Normalizer$ Normalizer]]
  * @see [[de.sciss.synth.ugen.Compander$ Compander]]
  */
object Limiter extends ProductReader[Limiter] {
  /** @param in               input signal to be limited
    * @param level            maximum amplitude to which the signal is limited. The
    *                         limiter will kick in when the input signal exceeds
    *                         `+level` or falls below `-level` .
    * @param dur              look-ahead time in seconds ''(init-time only)''
    */
  def kr(in: GE, level: GE = 1.0f, dur: GE = 0.01f): Limiter = new Limiter(control, in, level, dur)
  
  /** @param in               input signal to be limited
    * @param level            maximum amplitude to which the signal is limited. The
    *                         limiter will kick in when the input signal exceeds
    *                         `+level` or falls below `-level` .
    * @param dur              look-ahead time in seconds ''(init-time only)''
    */
  def ar(in: GE, level: GE = 1.0f, dur: GE = 0.01f): Limiter = new Limiter(audio, in, level, dur)
  
  def read(in: RefMapIn, arity: Int): Limiter = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _level  = in.readGE()
    val _dur    = in.readGE()
    new Limiter(_rate, _in, _level, _dur)
  }
}

/** Limits the input amplitude to the given level. Unlike `Compander` , this UGen
  * will never overshoot, but it needs to look ahead in the input signal,
  * introducing a delay in its output. The delay time is equal to twice the value of
  * the `dur` parameter (the buffer internally used).
  * 
  * @param in               input signal to be limited
  * @param level            maximum amplitude to which the signal is limited. The
  *                         limiter will kick in when the input signal exceeds
  *                         `+level` or falls below `-level` .
  * @param dur              look-ahead time in seconds ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.Normalizer$ Normalizer]]
  * @see [[de.sciss.synth.ugen.Compander$ Compander]]
  */
final case class Limiter(rate: Rate, in: GE, level: GE = 1.0f, dur: GE = 0.01f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, level.expand, dur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that normalizes the input amplitude to the given level. Unlike
  * `Compander` , this UGen will not overshoot, but it needs to look ahead in the
  * input signal, introducing a delay in its output. The delay time is equal to
  * twice the value of the `dur` parameter (the buffer internally used).
  * 
  * ===Examples===
  * 
  * {{{
  * // compare dry and wet
  * play {
  *   val z    = Decay2.ar(
  *     Impulse.ar(8, phase = LFSaw.kr(0.25) * 0.7),
  *     attack = 0.001, release = 0.3) * FSinOsc.ar(500)
  *   val in  = z * SinOsc.ar(0.05) * 0.5
  *   val flt = Normalizer.ar(in, dur = 0.15, level = 0.4)
  *   LinXFade2.ar(in, flt, MouseButton.kr(-1, 1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Limiter$ Limiter]]
  * @see [[de.sciss.synth.ugen.Compander$ Compander]]
  */
object Normalizer extends ProductReader[Normalizer] {
  /** @param in               input signal to be normalized
    * @param level            peak output amplitude level to which to normalize the
    *                         input
    * @param dur              look-ahead time in seconds. Shorter times will produce
    *                         smaller delays and quicker transient response times, but
    *                         may introduce amplitude modulation artifacts.
    *                         ''(init-time only)''
    */
  def kr(in: GE, level: GE = 1, dur: GE = 0.01f): Normalizer = new Normalizer(control, in, level, dur)
  
  /** @param in               input signal to be normalized
    * @param level            peak output amplitude level to which to normalize the
    *                         input
    * @param dur              look-ahead time in seconds. Shorter times will produce
    *                         smaller delays and quicker transient response times, but
    *                         may introduce amplitude modulation artifacts.
    *                         ''(init-time only)''
    */
  def ar(in: GE, level: GE = 1, dur: GE = 0.01f): Normalizer = new Normalizer(audio, in, level, dur)
  
  def read(in: RefMapIn, arity: Int): Normalizer = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _level  = in.readGE()
    val _dur    = in.readGE()
    new Normalizer(_rate, _in, _level, _dur)
  }
}

/** A UGen that normalizes the input amplitude to the given level. Unlike
  * `Compander` , this UGen will not overshoot, but it needs to look ahead in the
  * input signal, introducing a delay in its output. The delay time is equal to
  * twice the value of the `dur` parameter (the buffer internally used).
  * 
  * @param in               input signal to be normalized
  * @param level            peak output amplitude level to which to normalize the
  *                         input
  * @param dur              look-ahead time in seconds. Shorter times will produce
  *                         smaller delays and quicker transient response times, but
  *                         may introduce amplitude modulation artifacts.
  *                         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.Limiter$ Limiter]]
  * @see [[de.sciss.synth.ugen.Compander$ Compander]]
  */
final case class Normalizer(rate: Rate, in: GE, level: GE = 1, dur: GE = 0.01f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, level.expand, dur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** An amplitude follower UGen. Tracks and reports the peak amplitude of its input
  * signal.
  * 
  * ===Examples===
  * 
  * {{{
  * // use sound-card input to control pulse amplitude
  * play {
  *   // use headphones to prevent feedback!
  *   Pulse.ar(90, 0.3) * Amplitude.kr(PhysicalIn.ar(0))
  * }
  * }}}
  * {{{
  * // compare with known amplitude
  * play {
  *   val amp = MouseX.kr
  *   val in  = PinkNoise.ar(amp)
  *   val ana = Amplitude.kr(amp, attack = 2, release = 2)
  *   (ana - amp).poll(2, "discrepancy")
  *   in
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.DetectSilence$ DetectSilence]]
  */
object Amplitude extends ProductReader[Amplitude] {
  /** @param in               input signal to be analyzed
    * @param attack           60 dB convergence time in for following attacks, in
    *                         seconds
    * @param release          60 dB convergence time in for following decays, in
    *                         seconds
    */
  def kr(in: GE, attack: GE = 0.01f, release: GE = 0.01f): Amplitude = 
    new Amplitude(control, in, attack, release)
  
  /** @param in               input signal to be analyzed
    * @param attack           60 dB convergence time in for following attacks, in
    *                         seconds
    * @param release          60 dB convergence time in for following decays, in
    *                         seconds
    */
  def ar(in: GE, attack: GE = 0.01f, release: GE = 0.01f): Amplitude = 
    new Amplitude(audio, in, attack, release)
  
  def read(in: RefMapIn, arity: Int): Amplitude = {
    require (arity == 4)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _attack   = in.readGE()
    val _release  = in.readGE()
    new Amplitude(_rate, _in, _attack, _release)
  }
}

/** An amplitude follower UGen. Tracks and reports the peak amplitude of its input
  * signal.
  * 
  * @param in               input signal to be analyzed
  * @param attack           60 dB convergence time in for following attacks, in
  *                         seconds
  * @param release          60 dB convergence time in for following decays, in
  *                         seconds
  * 
  * @see [[de.sciss.synth.ugen.DetectSilence$ DetectSilence]]
  */
final case class Amplitude(rate: MaybeRate, in: GE, attack: GE = 0.01f, release: GE = 0.01f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, attack.expand, release.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen which detects whether its input signal falls below a given amplitude for
  * a given amount of time (becoming "silent"). A silence is detected if the
  * absolute sample values of the input remain less than or equal to the `amp`
  * threshold for a consecutive amount of time given by the `dur` argument.
  * 
  * A value of `1` is output when this condition is met, and a value of `0` is
  * output when the condition is not met (i.e. at least one sample occurs in the
  * input whose absolute value is greater than `amp` ). Besides, when the output
  * changes from zero to one, the `doneAction` is executed (unless it is `doNothing`
  * ).
  * 
  * A special case is the initial condition of the UGen: It will begin with an
  * output value of `0` (no silence detected), even if the input signal is below the
  * amplitude threshold. It is only after the first input sample rising above the
  * threshold that the actual monitoring begins and a trigger of `1` or the firing
  * of the done-action may occur.
  * 
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  */
object DetectSilence extends ProductReader[DetectSilence] {
  /** @param in               input signal to be measured.
    * @param amp              minimum amplitude threshold which must be exceeded for
    *                         the input signal to be considered non-silent.
    *                         ''(init-time only)''
    * @param dur              The duration in seconds for which the input signal must
    *                         be continuously smaller than or equal to the threshold
    *                         to be considered silent. ''(init-time only)''
    * @param doneAction       an action to be performed when the output changes from
    *                         zero to one (silence detected).
    */
  def kr(in: GE, amp: GE = 1.0E-4f, dur: GE = 0.1f, doneAction: GE = doNothing): DetectSilence = 
    new DetectSilence(control, in, amp, dur, doneAction)
  
  /** @param in               input signal to be measured.
    * @param amp              minimum amplitude threshold which must be exceeded for
    *                         the input signal to be considered non-silent.
    *                         ''(init-time only)''
    * @param dur              The duration in seconds for which the input signal must
    *                         be continuously smaller than or equal to the threshold
    *                         to be considered silent. ''(init-time only)''
    * @param doneAction       an action to be performed when the output changes from
    *                         zero to one (silence detected).
    */
  def ar(in: GE, amp: GE = 1.0E-4f, dur: GE = 0.1f, doneAction: GE = doNothing): DetectSilence = 
    new DetectSilence(audio, in, amp, dur, doneAction)
  
  def read(in: RefMapIn, arity: Int): DetectSilence = {
    require (arity == 5)
    val _rate       = in.readMaybeRate()
    val _in         = in.readGE()
    val _amp        = in.readGE()
    val _dur        = in.readGE()
    val _doneAction = in.readGE()
    new DetectSilence(_rate, _in, _amp, _dur, _doneAction)
  }
}

/** A UGen which detects whether its input signal falls below a given amplitude for
  * a given amount of time (becoming "silent"). A silence is detected if the
  * absolute sample values of the input remain less than or equal to the `amp`
  * threshold for a consecutive amount of time given by the `dur` argument.
  * 
  * A value of `1` is output when this condition is met, and a value of `0` is
  * output when the condition is not met (i.e. at least one sample occurs in the
  * input whose absolute value is greater than `amp` ). Besides, when the output
  * changes from zero to one, the `doneAction` is executed (unless it is `doNothing`
  * ).
  * 
  * A special case is the initial condition of the UGen: It will begin with an
  * output value of `0` (no silence detected), even if the input signal is below the
  * amplitude threshold. It is only after the first input sample rising above the
  * threshold that the actual monitoring begins and a trigger of `1` or the firing
  * of the done-action may occur.
  * 
  * @param in               input signal to be measured.
  * @param amp              minimum amplitude threshold which must be exceeded for
  *                         the input signal to be considered non-silent.
  *                         ''(init-time only)''
  * @param dur              The duration in seconds for which the input signal must
  *                         be continuously smaller than or equal to the threshold
  *                         to be considered silent. ''(init-time only)''
  * @param doneAction       an action to be performed when the output changes from
  *                         zero to one (silence detected).
  * 
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  */
final case class DetectSilence(rate: MaybeRate, in: GE, amp: GE = 1.0E-4f, dur: GE = 0.1f, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, amp.expand, dur.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1, hasSideEffect = true)
  }
}

/** A Hilbert transform UGen. This transformation produces two signals from a given
  * input with identical frequency content, but with their respective phases shifted
  * to be 90 degrees apart (0.5 pi radians).
  * 
  * The two signals output by `Hilbert` correspond to the real and imaginary part
  * of the complex transformed signal. Due to the method used (an IIR filter),
  * distortion occurs in the upper octave of the frequency spectrum.
  * 
  * The transform can be used to implemented single-side-band (SSB) modulation, but
  * a dedicated UGen `FreqShift` is already provided for this case.
  * 
  * ===Examples===
  * 
  * {{{
  * // a form of envelope tracking
  * play {
  *   val in = SinOsc.ar(440)
  *   val h  = Hilbert.ar(in)
  *   val x  = h.real.squared + h.imag.squared
  *   x.poll(1)  // cos(x)^2 + sin(x)^2 == 1 (ideally)
  *   0
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FreqShift$ FreqShift]]
  */
object Hilbert extends ProductReader[Hilbert] {
  /** @param in               input signal to be processed
    */
  def ar(in: GE): Hilbert = new Hilbert(audio, in)
  
  def read(in: RefMapIn, arity: Int): Hilbert = {
    require (arity == 2)
    val _rate = in.readRate()
    val _in   = in.readGE()
    new Hilbert(_rate, _in)
  }
}

/** A Hilbert transform UGen. This transformation produces two signals from a given
  * input with identical frequency content, but with their respective phases shifted
  * to be 90 degrees apart (0.5 pi radians).
  * 
  * The two signals output by `Hilbert` correspond to the real and imaginary part
  * of the complex transformed signal. Due to the method used (an IIR filter),
  * distortion occurs in the upper octave of the frequency spectrum.
  * 
  * The transform can be used to implemented single-side-band (SSB) modulation, but
  * a dedicated UGen `FreqShift` is already provided for this case.
  * 
  * @param in               input signal to be processed
  * 
  * @see [[de.sciss.synth.ugen.FreqShift$ FreqShift]]
  */
final case class Hilbert(rate: Rate, in: GE) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args)
  
  def real: GE = ChannelProxy(this, 0)
  
  def imag: GE = ChannelProxy(this, 1)
}

/** A frequency shifting UGen. It implements single sideband (SSB) amplitude
  * modulation, also known as frequency shifting, but not to be confused with pitch
  * shifting. Frequency shifting moves all the components of a signal by a fixed
  * amount but does not preserve the original harmonic relationships.
  * 
  * ===Examples===
  * 
  * {{{
  * // shift a sine frequency from 200 to 700 Hz
  * play {
  *   val freq = Line.ar(0, 500, 5)
  *   FreqShift.ar(SinOsc.ar(200) * 0.25, freq)
  * }
  * }}}
  * {{{
  * // negative frequency to shift downwards
  * play {
  *   val freq = Line.ar(0, -500, 5)
  *   FreqShift.ar(SinOsc.ar(700) * 0.25, freq)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Hilbert$ Hilbert]]
  * @see [[de.sciss.synth.ugen.PV_MagShift$ PV_MagShift]]
  */
object FreqShift extends ProductReader[FreqShift] {
  /** @param freq             the shift amount in Hertz. Positive values shift
    *                         upwards, negative values shift downwards.
    * @param phase            a phase parameter in radians (0 to 2 Pi).
    */
  def ar(in: GE, freq: GE = 0.0f, phase: GE = 0.0f): FreqShift = new FreqShift(audio, in, freq, phase)
  
  def read(in: RefMapIn, arity: Int): FreqShift = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _phase  = in.readGE()
    new FreqShift(_rate, _in, _freq, _phase)
  }
}

/** A frequency shifting UGen. It implements single sideband (SSB) amplitude
  * modulation, also known as frequency shifting, but not to be confused with pitch
  * shifting. Frequency shifting moves all the components of a signal by a fixed
  * amount but does not preserve the original harmonic relationships.
  * 
  * @param freq             the shift amount in Hertz. Positive values shift
  *                         upwards, negative values shift downwards.
  * @param phase            a phase parameter in radians (0 to 2 Pi).
  * 
  * @see [[de.sciss.synth.ugen.Hilbert$ Hilbert]]
  * @see [[de.sciss.synth.ugen.PV_MagShift$ PV_MagShift]]
  */
final case class FreqShift(rate: Rate, in: GE, freq: GE = 0.0f, phase: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, phase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A Moog VCF style UGen. This is a type of resonant low pass filter.
  * 
  * The design of this filter is described in Federico Fontana, "Preserving the
  * Digital Structure of the Moog VCF." In: Proceedings of the ICMC, Copenhagen
  * 2007. Ported to SuperCollider by Dan Stowell.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse controlled
  * play {
  *   val in   = WhiteNoise.ar(01.1)
  *   val freq = MouseY.kr(100, 10000, 1)
  *   val gain = MouseX.kr(0, 4)
  *   Limiter.ar(MoogFF.ar(in, freq, gain))
  * }
  * }}}
  */
object MoogFF extends ProductReader[MoogFF] {
  /** @param freq             cutoff frequency in Hertz
    * @param gain             filter resonance gain, between 0 and 4
    * @param reset            when greater than zero, this will reset the state of
    *                         the digital filters at the beginning of the next control
    *                         block.
    */
  def kr(in: GE, freq: GE = 200.0f, gain: GE = 2.0f, reset: GE = 0): MoogFF = 
    new MoogFF(control, in, freq, gain, reset)
  
  /** @param freq             cutoff frequency in Hertz
    * @param gain             filter resonance gain, between 0 and 4
    * @param reset            when greater than zero, this will reset the state of
    *                         the digital filters at the beginning of the next control
    *                         block.
    */
  def ar(in: GE, freq: GE = 200.0f, gain: GE = 2.0f, reset: GE = 0): MoogFF = 
    new MoogFF(audio, in, freq, gain, reset)
  
  def read(in: RefMapIn, arity: Int): MoogFF = {
    require (arity == 5)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _gain   = in.readGE()
    val _reset  = in.readGE()
    new MoogFF(_rate, _in, _freq, _gain, _reset)
  }
}

/** A Moog VCF style UGen. This is a type of resonant low pass filter.
  * 
  * The design of this filter is described in Federico Fontana, "Preserving the
  * Digital Structure of the Moog VCF." In: Proceedings of the ICMC, Copenhagen
  * 2007. Ported to SuperCollider by Dan Stowell.
  * 
  * @param freq             cutoff frequency in Hertz
  * @param gain             filter resonance gain, between 0 and 4
  * @param reset            when greater than zero, this will reset the state of
  *                         the digital filters at the beginning of the next control
  *                         block.
  */
final case class MoogFF(rate: MaybeRate, in: GE, freq: GE = 200.0f, gain: GE = 2.0f, reset: GE = 0)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, gain.expand, reset.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A 2nd order (12db per oct roll-off) resonant low pass filter UGen. The B
  * equalization suite is based on the Second Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BLowPass extends ProductReader[BLowPass] {
  /** @param in               input signal to be processed.
    * @param freq             cutoff frequency.
    * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
    */
  def ar(in: GE, freq: GE = 500.0f, rq: GE = 1.0f): BLowPass = new BLowPass(in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): BLowPass = {
    require (arity == 3)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new BLowPass(_in, _freq, _rq)
  }
}

/** A 2nd order (12db per oct roll-off) resonant low pass filter UGen. The B
  * equalization suite is based on the Second Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             cutoff frequency.
  * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
  */
final case class BLowPass(in: GE, freq: GE = 500.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** A 2nd order (12db per oct roll-off) resonant high pass filter UGen. The B
  * equalization suite is based on the Second Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BHiPass extends ProductReader[BHiPass] {
  /** @param in               input signal to be processed.
    * @param freq             cutoff frequency.
    * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
    */
  def ar(in: GE, freq: GE = 500.0f, rq: GE = 1.0f): BHiPass = new BHiPass(in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): BHiPass = {
    require (arity == 3)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new BHiPass(_in, _freq, _rq)
  }
}

/** A 2nd order (12db per oct roll-off) resonant high pass filter UGen. The B
  * equalization suite is based on the Second Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             cutoff frequency.
  * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
  */
final case class BHiPass(in: GE, freq: GE = 500.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** An band pass filter UGen. The B equalization suite is based on the Second Order
  * Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BBandPass extends ProductReader[BBandPass] {
  /** @param in               input signal to be processed.
    * @param freq             center frequency.
    * @param bw               the bandwidth '''in octaves''' between -3 dB frequencies
    */
  def ar(in: GE, freq: GE = 500.0f, bw: GE = 1.0f): BBandPass = new BBandPass(in, freq, bw)
  
  def read(in: RefMapIn, arity: Int): BBandPass = {
    require (arity == 3)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _bw   = in.readGE()
    new BBandPass(_in, _freq, _bw)
  }
}

/** An band pass filter UGen. The B equalization suite is based on the Second Order
  * Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             center frequency.
  * @param bw               the bandwidth '''in octaves''' between -3 dB frequencies
  */
final case class BBandPass(in: GE, freq: GE = 500.0f, bw: GE = 1.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, bw.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** An band stop (reject) filter UGen. The B equalization suite is based on the
  * Second Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BBandStop extends ProductReader[BBandStop] {
  /** @param in               input signal to be processed.
    * @param freq             center frequency.
    * @param bw               the bandwidth '''in octaves''' between -3 dB frequencies
    */
  def ar(in: GE, freq: GE = 500.0f, bw: GE = 1.0f): BBandStop = new BBandStop(in, freq, bw)
  
  def read(in: RefMapIn, arity: Int): BBandStop = {
    require (arity == 3)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _bw   = in.readGE()
    new BBandStop(_in, _freq, _bw)
  }
}

/** An band stop (reject) filter UGen. The B equalization suite is based on the
  * Second Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             center frequency.
  * @param bw               the bandwidth '''in octaves''' between -3 dB frequencies
  */
final case class BBandStop(in: GE, freq: GE = 500.0f, bw: GE = 1.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, bw.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** An parametric equalizer UGen. The B equalization suite is based on the Second
  * Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BPeakEQ extends ProductReader[BPeakEQ] {
  /** @param in               input signal to be processed.
    * @param freq             center frequency.
    * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
    * @param gain             boost/cut at the center frequency (in decibels).
    */
  def ar(in: GE, freq: GE = 500.0f, rq: GE = 1.0f, gain: GE = 0.0f): BPeakEQ = 
    new BPeakEQ(in, freq, rq, gain)
  
  def read(in: RefMapIn, arity: Int): BPeakEQ = {
    require (arity == 4)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    val _gain = in.readGE()
    new BPeakEQ(_in, _freq, _rq, _gain)
  }
}

/** An parametric equalizer UGen. The B equalization suite is based on the Second
  * Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             center frequency.
  * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
  * @param gain             boost/cut at the center frequency (in decibels).
  */
final case class BPeakEQ(in: GE, freq: GE = 500.0f, rq: GE = 1.0f, gain: GE = 0.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand, gain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** An all pass filter UGen. The B equalization suite is based on the Second Order
  * Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BAllPass extends ProductReader[BAllPass] {
  /** @param in               input signal to be processed.
    * @param freq             cutoff frequency.
    * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
    */
  def ar(in: GE, freq: GE = 500.0f, rq: GE = 1.0f): BAllPass = new BAllPass(in, freq, rq)
  
  def read(in: RefMapIn, arity: Int): BAllPass = {
    require (arity == 3)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rq   = in.readGE()
    new BAllPass(_in, _freq, _rq)
  }
}

/** An all pass filter UGen. The B equalization suite is based on the Second Order
  * Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             cutoff frequency.
  * @param rq               the reciprocal of Q, hence bandwidth / cutoffFreq.
  */
final case class BAllPass(in: GE, freq: GE = 500.0f, rq: GE = 1.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** A low shelf equalizer UGen. The B equalization suite is based on the Second
  * Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BLowShelf extends ProductReader[BLowShelf] {
  /** @param in               input signal to be processed.
    * @param freq             cutoff frequency.
    * @param rs               the reciprocal of the slope S (Shell boost/cut slope).
    *                         When `S = 1` , the shelf slope is as steep as it can be
    *                         and remain monotonically increasing or decreasing gain
    *                         with frequency. The shelf slope, in dB/octave, remains
    *                         proportional to S for all other values for a fixed
    *                         freq/sample-rate and `gain` .
    * @param gain             boost/cut at the cutoff frequency (in decibels).
    */
  def ar(in: GE, freq: GE = 500.0f, rs: GE = 1.0f, gain: GE = 0.0f): BLowShelf = 
    new BLowShelf(in, freq, rs, gain)
  
  def read(in: RefMapIn, arity: Int): BLowShelf = {
    require (arity == 4)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rs   = in.readGE()
    val _gain = in.readGE()
    new BLowShelf(_in, _freq, _rs, _gain)
  }
}

/** A low shelf equalizer UGen. The B equalization suite is based on the Second
  * Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             cutoff frequency.
  * @param rs               the reciprocal of the slope S (Shell boost/cut slope).
  *                         When `S = 1` , the shelf slope is as steep as it can be
  *                         and remain monotonically increasing or decreasing gain
  *                         with frequency. The shelf slope, in dB/octave, remains
  *                         proportional to S for all other values for a fixed
  *                         freq/sample-rate and `gain` .
  * @param gain             boost/cut at the cutoff frequency (in decibels).
  */
final case class BLowShelf(in: GE, freq: GE = 500.0f, rs: GE = 1.0f, gain: GE = 0.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rs.expand, gain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** A high shelf equalizer UGen. The B equalization suite is based on the Second
  * Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  */
object BHiShelf extends ProductReader[BHiShelf] {
  /** @param in               input signal to be processed.
    * @param freq             cutoff frequency.
    * @param rs               the reciprocal of the slope S (Shell boost/cut slope).
    *                         When `S = 1` , the shelf slope is as steep as it can be
    *                         and remain monotonically increasing or decreasing gain
    *                         with frequency. The shelf slope, in dB/octave, remains
    *                         proportional to S for all other values for a fixed
    *                         freq/sample-rate and `gain` .
    * @param gain             boost/cut at the cutoff frequency (in decibels).
    */
  def ar(in: GE, freq: GE = 500.0f, rs: GE = 1.0f, gain: GE = 0.0f): BHiShelf = 
    new BHiShelf(in, freq, rs, gain)
  
  def read(in: RefMapIn, arity: Int): BHiShelf = {
    require (arity == 4)
    val _in   = in.readGE()
    val _freq = in.readGE()
    val _rs   = in.readGE()
    val _gain = in.readGE()
    new BHiShelf(_in, _freq, _rs, _gain)
  }
}

/** A high shelf equalizer UGen. The B equalization suite is based on the Second
  * Order Section (SOS) biquad UGen.
  * 
  * Note: Biquad coefficient calculations imply certain amount of CPU overhead.
  * These plugin UGens contain optimizations such that the coefficients get updated
  * only when there has been a change to one of the filter's parameters. This can
  * cause spikes in CPU performance and should be considered when using several of
  * these units.
  * 
  * @param in               input signal to be processed.
  * @param freq             cutoff frequency.
  * @param rs               the reciprocal of the slope S (Shell boost/cut slope).
  *                         When `S = 1` , the shelf slope is as steep as it can be
  *                         and remain monotonically increasing or decreasing gain
  *                         with frequency. The shelf slope, in dB/octave, remains
  *                         proportional to S for all other values for a fixed
  *                         freq/sample-rate and `gain` .
  * @param gain             boost/cut at the cutoff frequency (in decibels).
  */
final case class BHiShelf(in: GE, freq: GE = 500.0f, rs: GE = 1.0f, gain: GE = 0.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, rs.expand, gain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}