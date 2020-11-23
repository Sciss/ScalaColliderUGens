// revision: 1
package de.sciss.synth
package ugen

import UGenSource._

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a - b * sqrt(abs(x[n]))
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // vary frequency
  * play { CuspN.ar(MouseX.kr(20, SampleRate.ir), 1.0, 1.99) * 0.3 }
  * }}}
  * {{{
  * // mouse-controlled parameters
  * play { CuspN.ar(SampleRate.ir/4, MouseX.kr(0.9, 1.1, 1), MouseY.kr(1.8, 2, 1)) * 0.3 }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(CuspN.ar(40, MouseX.kr(0.9, 1.1, 1), MouseY.kr(1.8, 2,1)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.CuspL$ CuspL]]
  */
object CuspN {
  def ar: CuspN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 1.9f, xi: GE = 0.0f): CuspN = 
    new CuspN(audio, freq, a, b, xi)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a - b * sqrt(abs(x[n]))
  * }}}
  * 
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.CuspL$ CuspL]]
  */
final case class CuspN(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 1.9f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a - b * sqrt(abs(x[n]))
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // vary frequency
  * play { CuspL.ar(MouseX.kr(20, SampleRate.ir), 1.0, 1.99) * 0.3 }
  * }}}
  * {{{
  * // mouse-controlled parameters
  * play { CuspL.ar(SampleRate.ir/4, MouseX.kr(0.9, 1.1, 1), MouseY.kr(1.8, 2,1)) * 0.3 }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(CuspL.ar(40, MouseX.kr(0.9, 1.1, 1), MouseY.kr(1.8, 2, 1)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.CuspN$ CuspN]]
  */
object CuspL {
  def ar: CuspL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 1.9f, xi: GE = 0.0f): CuspL = 
    new CuspL(audio, freq, a, b, xi)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a - b * sqrt(abs(x[n]))
  * }}}
  * 
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.CuspN$ CuspN]]
  */
final case class CuspL(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 1.9f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = sin(im * y[n] + fb * x[n])
  * y[n+1] = (a * y[n] + c) % 2pi
  * }}}
  * This uses a linear congruential function to drive the phase indexing of a sine
  * wave. For im = 1, fb = 0, and a = 1 a normal sine wave results.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { FBSineN.ar(SampleRate.ir/4) * 0.2 }
  * }}}
  * {{{
  * // increase feedback
  * play { FBSineN.ar(SampleRate.ir, 1, Line.kr(0.01, 4, 10), 1, 0.1) * 0.2 }
  * }}}
  * {{{
  * // increase phase multiplier
  * play { FBSineN.ar(SampleRate.ir, 1, 0, XLine.kr(1, 2, 10), 0.1) * 0.2 }
  * }}}
  * {{{
  * // modulate frequency and index multiplier
  * play { FBSineN.ar(LFNoise2.kr(1).mulAdd(1e4, 1e4), LFNoise2.kr(1).mulAdd(16, 17), 1, 1.005, 0.7) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   FBSineN.ar(
  *     LFNoise2.kr(1).mulAdd(1e4, 1e4),
  *     LFNoise2.kr(1).mulAdd(32, 33),
  *     LFNoise2.kr(1) * 0.5,
  *     LFNoise2.kr(1).mulAdd(0.05, 1.05),
  *     LFNoise2.kr(1).mulAdd(0.3, 0.3)
  *   ) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FBSineL$ FBSineL]]
  * @see [[de.sciss.synth.ugen.FBSineC$ FBSineC]]
  */
object FBSineN {
  def ar: FBSineN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param im               Index multiplier amount
    * @param fb               Feedback amount
    * @param a                Phase multiplier amount
    * @param c                Phase increment amount
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), im: GE = 1.0f, fb: GE = 0.1f, a: GE = 1.1f, c: GE = 0.5f, xi: GE = 0.1f, yi: GE = 0.1f): FBSineN = 
    new FBSineN(audio, freq, im, fb, a, c, xi, yi)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = sin(im * y[n] + fb * x[n])
  * y[n+1] = (a * y[n] + c) % 2pi
  * }}}
  * This uses a linear congruential function to drive the phase indexing of a sine
  * wave. For im = 1, fb = 0, and a = 1 a normal sine wave results.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param im               Index multiplier amount
  * @param fb               Feedback amount
  * @param a                Phase multiplier amount
  * @param c                Phase increment amount
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.FBSineL$ FBSineL]]
  * @see [[de.sciss.synth.ugen.FBSineC$ FBSineC]]
  */
final case class FBSineN(rate: Rate, freq: GE = Nyquist(), im: GE = 1.0f, fb: GE = 0.1f, a: GE = 1.1f, c: GE = 0.5f, xi: GE = 0.1f, yi: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, im.expand, fb.expand, a.expand, c.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = sin(im * y[n] + fb * x[n])
  * y[n+1] = (a * y[n] + c) % 2pi
  * }}}
  * This uses a linear congruential function to drive the phase indexing of a sine
  * wave. For im = 1, fb = 0, and a = 1 a normal sine wave results.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { FBSineL.ar(SampleRate.ir/4) * 0.2 }
  * }}}
  * {{{
  * // increase feedback
  * play { FBSineL.ar(SampleRate.ir, 1, Line.kr(0.01, 4, 10), 1, 0.1) * 0.2 }
  * }}}
  * {{{
  * // increase phase multiplier
  * play { FBSineL.ar(SampleRate.ir, 1, 0, XLine.kr(1, 2, 10), 0.1) * 0.2 }
  * }}}
  * {{{
  * // modulate frequency and index multiplier
  * play { FBSineL.ar(LFNoise2.kr(1).mulAdd(1e4, 1e4), LFNoise2.kr(1).mulAdd(16, 17), 1, 1.005, 0.7) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   FBSineL.ar(
  *     LFNoise2.kr(1).mulAdd(1e4, 1e4),
  *     LFNoise2.kr(1).mulAdd(32, 33),
  *     LFNoise2.kr(1) * 0.5,
  *     LFNoise2.kr(1).mulAdd(0.05, 1.05),
  *     LFNoise2.kr(1).mulAdd(0.3, 0.3)
  *   ) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FBSineN$ FBSineN]]
  * @see [[de.sciss.synth.ugen.FBSineC$ FBSineC]]
  */
object FBSineL {
  def ar: FBSineL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param im               Index multiplier amount
    * @param fb               Feedback amount
    * @param a                Phase multiplier amount
    * @param c                Phase increment amount
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), im: GE = 1.0f, fb: GE = 0.1f, a: GE = 1.1f, c: GE = 0.5f, xi: GE = 0.1f, yi: GE = 0.1f): FBSineL = 
    new FBSineL(audio, freq, im, fb, a, c, xi, yi)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = sin(im * y[n] + fb * x[n])
  * y[n+1] = (a * y[n] + c) % 2pi
  * }}}
  * This uses a linear congruential function to drive the phase indexing of a sine
  * wave. For im = 1, fb = 0, and a = 1 a normal sine wave results.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param im               Index multiplier amount
  * @param fb               Feedback amount
  * @param a                Phase multiplier amount
  * @param c                Phase increment amount
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.FBSineN$ FBSineN]]
  * @see [[de.sciss.synth.ugen.FBSineC$ FBSineC]]
  */
final case class FBSineL(rate: Rate, freq: GE = Nyquist(), im: GE = 1.0f, fb: GE = 0.1f, a: GE = 1.1f, c: GE = 0.5f, xi: GE = 0.1f, yi: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, im.expand, fb.expand, a.expand, c.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = sin(im * y[n] + fb * x[n])
  * y[n+1] = (a * y[n] + c) % 2pi
  * }}}
  * This uses a linear congruential function to drive the phase indexing of a sine
  * wave. For im = 1, fb = 0 , and a = 1 a normal sine wave results.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { FBSineC.ar(SampleRate.ir/4) * 0.2 }
  * }}}
  * {{{
  * // increase feedback
  * play { FBSineC.ar(SampleRate.ir, 1, Line.kr(0.01, 4, 10), 1, 0.1) * 0.2 }
  * }}}
  * {{{
  * // increase phase multiplier
  * play { FBSineC.ar(SampleRate.ir, 1, 0, XLine.kr(1, 2, 10), 0.1) * 0.2 }
  * }}}
  * {{{
  * // modulate frequency and index multiplier
  * play { FBSineC.ar(LFNoise2.kr(1).mulAdd(1e4, 1e4), LFNoise2.kr(1).mulAdd(16, 17), 1, 1.005, 0.7) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   FBSineC.ar(
  *     LFNoise2.kr(1).mulAdd(1e4, 1e4),
  *     LFNoise2.kr(1).mulAdd(32, 33),
  *     LFNoise2.kr(1) * 0.5,
  *     LFNoise2.kr(1).mulAdd(0.05, 1.05),
  *     LFNoise2.kr(1).mulAdd(0.3, 0.3)
  *   ) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FBSineN$ FBSineN]]
  * @see [[de.sciss.synth.ugen.FBSineL$ FBSineL]]
  */
object FBSineC {
  def ar: FBSineC = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param im               Index multiplier amount
    * @param fb               Feedback amount
    * @param a                Phase multiplier amount
    * @param c                Phase increment amount
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), im: GE = 1.0f, fb: GE = 0.1f, a: GE = 1.1f, c: GE = 0.5f, xi: GE = 0.1f, yi: GE = 0.1f): FBSineC = 
    new FBSineC(audio, freq, im, fb, a, c, xi, yi)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = sin(im * y[n] + fb * x[n])
  * y[n+1] = (a * y[n] + c) % 2pi
  * }}}
  * This uses a linear congruential function to drive the phase indexing of a sine
  * wave. For im = 1, fb = 0 , and a = 1 a normal sine wave results.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param im               Index multiplier amount
  * @param fb               Feedback amount
  * @param a                Phase multiplier amount
  * @param c                Phase increment amount
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.FBSineN$ FBSineN]]
  * @see [[de.sciss.synth.ugen.FBSineL$ FBSineL]]
  */
final case class FBSineC(rate: Rate, freq: GE = Nyquist(), im: GE = 1.0f, fb: GE = 0.1f, a: GE = 1.1f, c: GE = 0.5f, xi: GE = 0.1f, yi: GE = 0.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, im.expand, fb.expand, a.expand, c.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = 1 - y[n] + abs(x[n])
  * y[n+1] = x[n]
  * }}}
  * The behavior of the system is only dependent on its initial conditions.
  * Reference: Devaney, R. L. "The Gingerbreadman." Algorithm 3, 15-16, Jan. 1992.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { GbmanN.ar(MouseX.kr(20, SampleRate.ir)) * 0.1 }
  * }}}
  * {{{
  * // change initial parameters
  * play { GbmanN.ar(MouseX.kr(20, SampleRate.ir), -0.7, -2.7) * 0.1 }
  * }}}
  * {{{
  * // wait for it...
  * play { GbmanN.ar(MouseX.kr(20, SampleRate.ir), 1.2, 2.0002) * 0.1 }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(GbmanN.ar(40) * 400 + 500) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.GbmanL$ GbmanL]]
  */
object GbmanN {
  def ar: GbmanN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), xi: GE = 1.2f, yi: GE = 2.1f): GbmanN = new GbmanN(audio, freq, xi, yi)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = 1 - y[n] + abs(x[n])
  * y[n+1] = x[n]
  * }}}
  * The behavior of the system is only dependent on its initial conditions.
  * Reference: Devaney, R. L. "The Gingerbreadman." Algorithm 3, 15-16, Jan. 1992.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.GbmanL$ GbmanL]]
  */
final case class GbmanN(rate: Rate, freq: GE = Nyquist(), xi: GE = 1.2f, yi: GE = 2.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}
object GbmanL {
  def ar: GbmanL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), xi: GE = 1.2f, yi: GE = 2.1f): GbmanL = new GbmanL(audio, freq, xi, yi)
}

/** @param freq             Iteration frequency in Hertz
  * @param yi               Initial value of y
  */
final case class GbmanL(rate: Rate, freq: GE = Nyquist(), xi: GE = 1.2f, yi: GE = 2.1f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+2] = 1 - a * pow(x[n+1], 2) + b * x[n]
  * }}}
  * This equation was discovered by French astronomer Michel Hénon while studying
  * the orbits of stars in globular clusters.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { HenonN.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // mouse-control of parameters
  * play { HenonN.ar(SampleRate.ir/4, MouseX.kr(1,1.4), MouseY.kr(0,0.3)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   HenonN.ar(
  *     SampleRate.ir/8,
  *     LFNoise2.kr(1).mulAdd(0.2, 1.2),
  *     LFNoise2.kr(1).mulAdd(0.15, 0.15)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(HenonN.ar(40, MouseX.kr(1, 1.4), MouseY.kr(0, 0.3)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HenonL$ HenonL]]
  * @see [[de.sciss.synth.ugen.HenonC$ HenonC]]
  */
object HenonN {
  def ar: HenonN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param x0               Initial value of x
    * @param x1               Second value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.4f, b: GE = 0.3f, x0: GE = 0.0f, x1: GE = 0.0f): HenonN = 
    new HenonN(audio, freq, a, b, x0, x1)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+2] = 1 - a * pow(x[n+1], 2) + b * x[n]
  * }}}
  * This equation was discovered by French astronomer Michel Hénon while studying
  * the orbits of stars in globular clusters.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param x0               Initial value of x
  * @param x1               Second value of x
  * 
  * @see [[de.sciss.synth.ugen.HenonL$ HenonL]]
  * @see [[de.sciss.synth.ugen.HenonC$ HenonC]]
  */
final case class HenonN(rate: Rate, freq: GE = Nyquist(), a: GE = 1.4f, b: GE = 0.3f, x0: GE = 0.0f, x1: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, x0.expand, x1.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+2] = 1 - a * pow(x[n+1], 2) + b * x[n]
  * }}}
  * This equation was discovered by French astronomer Michel Hénon while studying
  * the orbits of stars in globular clusters.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { HenonL.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // mouse-control of parameters
  * play { HenonL.ar(SampleRate.ir/4, MouseX.kr(1,1.4), MouseY.kr(0,0.3)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   HenonL.ar(
  *     SampleRate.ir/8,
  *     LFNoise2.kr(1).mulAdd(0.2, 1.2),
  *     LFNoise2.kr(1).mulAdd(0.15, 0.15)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(HenonL.ar(40, MouseX.kr(1, 1.4), MouseY.kr(0, 0.3)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HenonL$ HenonL]]
  * @see [[de.sciss.synth.ugen.HenonC$ HenonC]]
  */
object HenonL {
  def ar: HenonL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param x0               Initial value of x
    * @param x1               Second value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.4f, b: GE = 0.3f, x0: GE = 0.0f, x1: GE = 0.0f): HenonL = 
    new HenonL(audio, freq, a, b, x0, x1)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+2] = 1 - a * pow(x[n+1], 2) + b * x[n]
  * }}}
  * This equation was discovered by French astronomer Michel Hénon while studying
  * the orbits of stars in globular clusters.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param x0               Initial value of x
  * @param x1               Second value of x
  * 
  * @see [[de.sciss.synth.ugen.HenonL$ HenonL]]
  * @see [[de.sciss.synth.ugen.HenonC$ HenonC]]
  */
final case class HenonL(rate: Rate, freq: GE = Nyquist(), a: GE = 1.4f, b: GE = 0.3f, x0: GE = 0.0f, x1: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, x0.expand, x1.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A cubic-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+2] = 1 - a * pow(x[n+1], 2) + b * x[n]
  * }}}
  * This equation was discovered by French astronomer Michel Hénon while studying
  * the orbits of stars in globular clusters.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { HenonC.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // mouse-control of parameters
  * play { HenonC.ar(SampleRate.ir/4, MouseX.kr(1,1.4), MouseY.kr(0,0.3)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   HenonC.ar(
  *     SampleRate.ir/8,
  *     LFNoise2.kr(1).mulAdd(0.2, 1.2),
  *     LFNoise2.kr(1).mulAdd(0.15, 0.15)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(HenonC.ar(40, MouseX.kr(1, 1.4), MouseY.kr(0, 0.3)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.HenonL$ HenonL]]
  * @see [[de.sciss.synth.ugen.HenonC$ HenonC]]
  */
object HenonC {
  def ar: HenonC = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param x0               Initial value of x
    * @param x1               Second value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.4f, b: GE = 0.3f, x0: GE = 0.0f, x1: GE = 0.0f): HenonC = 
    new HenonC(audio, freq, a, b, x0, x1)
}

/** A cubic-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+2] = 1 - a * pow(x[n+1], 2) + b * x[n]
  * }}}
  * This equation was discovered by French astronomer Michel Hénon while studying
  * the orbits of stars in globular clusters.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param x0               Initial value of x
  * @param x1               Second value of x
  * 
  * @see [[de.sciss.synth.ugen.HenonL$ HenonL]]
  * @see [[de.sciss.synth.ugen.HenonC$ HenonC]]
  */
final case class HenonC(rate: Rate, freq: GE = Nyquist(), a: GE = 1.4f, b: GE = 0.3f, x0: GE = 0.0f, x1: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, x0.expand, x1.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on a function given in Clifford
  * Pickover's book Chaos In Wonderland, pg 26. The function is:
  * {{{
  * x[n+1] = sin(b * y[n]) + c * sin(b * x[n])
  * y[n+1] = sin(a * y[n]) + d * sin(a * x[n])
  * }}}
  * According to Pickover, parameters a and b should be in the range from -3 to +3,
  * and parameters c and d should be in the range from 0.5 to 1.5. The function can,
  * depending on the parameters given, give continuous chaotic output, converge to a
  * single value (silence) or oscillate in a cycle (tone). NOTE: This UGen is
  * experimental and not optimized currently, so is rather hoggish of CPU.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { LatoocarfianN.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate all parameters
  * play {
  *   LatoocarfianN.ar(
  *     SampleRate.ir/4,
  *     LFNoise2.kr(2).mulAdd(1.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(1.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(0.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(0.5, 1.5)
  *   ) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LatoocarfianL$ LatoocarfianL]]
  * @see [[de.sciss.synth.ugen.LatoocarfianC$ LatoocarfianC]]
  */
object LatoocarfianN {
  def ar: LatoocarfianN = ar()
  
  /** @param freq             Iteration frequency in Hertz.
    * @param a                Equation variable
    * @param b                Equation variable
    * @param c                Equation variable
    * @param d                Equation variable
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 3.0f, c: GE = 0.5f, d: GE = 0.5f, xi: GE = 0.5f, yi: GE = 0.5f): LatoocarfianN = 
    new LatoocarfianN(audio, freq, a, b, c, d, xi, yi)
}

/** A non-interpolating sound generator based on a function given in Clifford
  * Pickover's book Chaos In Wonderland, pg 26. The function is:
  * {{{
  * x[n+1] = sin(b * y[n]) + c * sin(b * x[n])
  * y[n+1] = sin(a * y[n]) + d * sin(a * x[n])
  * }}}
  * According to Pickover, parameters a and b should be in the range from -3 to +3,
  * and parameters c and d should be in the range from 0.5 to 1.5. The function can,
  * depending on the parameters given, give continuous chaotic output, converge to a
  * single value (silence) or oscillate in a cycle (tone). NOTE: This UGen is
  * experimental and not optimized currently, so is rather hoggish of CPU.
  * 
  * @param freq             Iteration frequency in Hertz.
  * @param a                Equation variable
  * @param b                Equation variable
  * @param c                Equation variable
  * @param d                Equation variable
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.LatoocarfianL$ LatoocarfianL]]
  * @see [[de.sciss.synth.ugen.LatoocarfianC$ LatoocarfianC]]
  */
final case class LatoocarfianN(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 3.0f, c: GE = 0.5f, d: GE = 0.5f, xi: GE = 0.5f, yi: GE = 0.5f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, c.expand, d.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear-interpolating sound generator based on a function given in Clifford
  * Pickover's book Chaos In Wonderland, pg 26. The function is:
  * {{{
  * x[n+1] = sin(b * y[n]) + c * sin(b * x[n])
  * y[n+1] = sin(a * y[n]) + d * sin(a * x[n])
  * }}}
  * According to Pickover, parameters a and b should be in the range from -3 to +3,
  * and parameters c and d should be in the range from 0.5 to 1.5. The function can,
  * depending on the parameters given, give continuous chaotic output, converge to a
  * single value (silence) or oscillate in a cycle (tone). NOTE: This UGen is
  * experimental and not optimized currently, so is rather hoggish of CPU.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { LatoocarfianL.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate all parameters
  * play {
  *   LatoocarfianL.ar(
  *     SampleRate.ir/4,
  *     LFNoise2.kr(2).mulAdd(1.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(1.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(0.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(0.5, 1.5)
  *   ) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  * @see [[de.sciss.synth.ugen.LatoocarfianC$ LatoocarfianC]]
  */
object LatoocarfianL {
  def ar: LatoocarfianL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param c                Equation variable
    * @param d                Equation variable
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 3.0f, c: GE = 0.5f, d: GE = 0.5f, xi: GE = 0.5f, yi: GE = 0.5f): LatoocarfianL = 
    new LatoocarfianL(audio, freq, a, b, c, d, xi, yi)
}

/** A linear-interpolating sound generator based on a function given in Clifford
  * Pickover's book Chaos In Wonderland, pg 26. The function is:
  * {{{
  * x[n+1] = sin(b * y[n]) + c * sin(b * x[n])
  * y[n+1] = sin(a * y[n]) + d * sin(a * x[n])
  * }}}
  * According to Pickover, parameters a and b should be in the range from -3 to +3,
  * and parameters c and d should be in the range from 0.5 to 1.5. The function can,
  * depending on the parameters given, give continuous chaotic output, converge to a
  * single value (silence) or oscillate in a cycle (tone). NOTE: This UGen is
  * experimental and not optimized currently, so is rather hoggish of CPU.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param c                Equation variable
  * @param d                Equation variable
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  * @see [[de.sciss.synth.ugen.LatoocarfianC$ LatoocarfianC]]
  */
final case class LatoocarfianL(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 3.0f, c: GE = 0.5f, d: GE = 0.5f, xi: GE = 0.5f, yi: GE = 0.5f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, c.expand, d.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A cubic-interpolating sound generator based on a function given in Clifford
  * Pickover's book Chaos In Wonderland, pg 26. The function is:
  * {{{
  * x[n+1] = sin(b * y[n]) + c * sin(b * x[n])
  * y[n+1] = sin(a * y[n]) + d * sin(a * x[n])
  * }}}
  * According to Pickover, parameters a and b should be in the range from -3 to +3,
  * and parameters c and d should be in the range from 0.5 to 1.5. The function can,
  * depending on the parameters given, give continuous chaotic output, converge to a
  * single value (silence) or oscillate in a cycle (tone). NOTE: This UGen is
  * experimental and not optimized currently, so is rather hoggish of CPU.
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { LatoocarfianC.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate all parameters
  * play {
  *   LatoocarfianC.ar(
  *     SampleRate.ir/4,
  *     LFNoise2.kr(2).mulAdd(1.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(1.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(0.5, 1.5),
  *     LFNoise2.kr(2).mulAdd(0.5, 1.5)
  *   ) * 0.2
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  * @see [[de.sciss.synth.ugen.LatoocarfianL$ LatoocarfianL]]
  */
object LatoocarfianC {
  def ar: LatoocarfianC = ar()
  
  /** @param freq             Iteration frequency in Hertz.
    * @param a                Equation variable
    * @param b                Equation variable
    * @param c                Equation variable
    * @param d                Equation variable
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 3.0f, c: GE = 0.5f, d: GE = 0.5f, xi: GE = 0.5f, yi: GE = 0.5f): LatoocarfianC = 
    new LatoocarfianC(audio, freq, a, b, c, d, xi, yi)
}

/** A cubic-interpolating sound generator based on a function given in Clifford
  * Pickover's book Chaos In Wonderland, pg 26. The function is:
  * {{{
  * x[n+1] = sin(b * y[n]) + c * sin(b * x[n])
  * y[n+1] = sin(a * y[n]) + d * sin(a * x[n])
  * }}}
  * According to Pickover, parameters a and b should be in the range from -3 to +3,
  * and parameters c and d should be in the range from 0.5 to 1.5. The function can,
  * depending on the parameters given, give continuous chaotic output, converge to a
  * single value (silence) or oscillate in a cycle (tone). NOTE: This UGen is
  * experimental and not optimized currently, so is rather hoggish of CPU.
  * 
  * @param freq             Iteration frequency in Hertz.
  * @param a                Equation variable
  * @param b                Equation variable
  * @param c                Equation variable
  * @param d                Equation variable
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.LatoocarfianN$ LatoocarfianN]]
  * @see [[de.sciss.synth.ugen.LatoocarfianL$ LatoocarfianL]]
  */
final case class LatoocarfianC(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = 3.0f, c: GE = 0.5f, d: GE = 0.5f, xi: GE = 0.5f, yi: GE = 0.5f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, c.expand, d.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = (a * x[n] + c) % m
  * }}}
  *  The output signal is automatically scaled to a range of [-1, 1].
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { LinCongN.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   LinCongN.ar(
  *     LFNoise2.kr(1.0).mulAdd(1e4, 1e4),
  *     LFNoise2.kr(0.1).mulAdd(0.5, 1.4),
  *     LFNoise2.kr(0.1).mulAdd(0.1, 0.1),
  *     LFNoise2.kr(0.1)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as frequency control
  * play {
  *   SinOsc.ar(
  *     LinCongN.ar(
  *       40,
  *       LFNoise2.kr(0.1).mulAdd(0.1, 1),
  *       LFNoise2.kr(0.1).mulAdd(0.1, 0.1),
  *       LFNoise2.kr(0.1)
  *     ).mulAdd(500, 600)
  *   ) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LinCongL$ LinCongL]]
  * @see [[de.sciss.synth.ugen.LinCongC$ LinCongC]]
  */
object LinCongN {
  def ar: LinCongN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Multiplier amount
    * @param c                Increment amount
    * @param m                Modulus amount
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.1f, c: GE = 0.13f, m: GE = 1.0f, xi: GE = 0.0f): LinCongN = 
    new LinCongN(audio, freq, a, c, m, xi)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = (a * x[n] + c) % m
  * }}}
  *  The output signal is automatically scaled to a range of [-1, 1].
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Multiplier amount
  * @param c                Increment amount
  * @param m                Modulus amount
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.LinCongL$ LinCongL]]
  * @see [[de.sciss.synth.ugen.LinCongC$ LinCongC]]
  */
final case class LinCongN(rate: Rate, freq: GE = Nyquist(), a: GE = 1.1f, c: GE = 0.13f, m: GE = 1.0f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, c.expand, m.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = (a * x[n] + c) % m
  * }}}
  *  The output signal is automatically scaled to a range of [-1, 1].
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { LinCongL.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   LinCongL.ar(
  *     LFNoise2.kr(1.0).mulAdd(1e4, 1e4),
  *     LFNoise2.kr(0.1).mulAdd(0.5, 1.4),
  *     LFNoise2.kr(0.1).mulAdd(0.1, 0.1),
  *     LFNoise2.kr(0.1)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as frequency control
  * play {
  *   SinOsc.ar(
  *     LinCongL.ar(
  *       40,
  *       LFNoise2.kr(0.1).mulAdd(0.1, 1),
  *       LFNoise2.kr(0.1).mulAdd(0.1, 0.1),
  *       LFNoise2.kr(0.1)
  *     ).mulAdd(500, 600)
  *   ) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LinCongN$ LinCongN]]
  * @see [[de.sciss.synth.ugen.LinCongC$ LinCongC]]
  */
object LinCongL {
  def ar: LinCongL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Multiplier amount
    * @param c                Increment amount
    * @param m                Modulus amount
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.1f, c: GE = 0.13f, m: GE = 1.0f, xi: GE = 0.0f): LinCongL = 
    new LinCongL(audio, freq, a, c, m, xi)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = (a * x[n] + c) % m
  * }}}
  *  The output signal is automatically scaled to a range of [-1, 1].
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Multiplier amount
  * @param c                Increment amount
  * @param m                Modulus amount
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.LinCongN$ LinCongN]]
  * @see [[de.sciss.synth.ugen.LinCongC$ LinCongC]]
  */
final case class LinCongL(rate: Rate, freq: GE = Nyquist(), a: GE = 1.1f, c: GE = 0.13f, m: GE = 1.0f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, c.expand, m.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A cubic-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = (a * x[n] + c) % m
  * }}}
  *  The output signal is automatically scaled to a range of [-1, 1].
  * 
  * ===Examples===
  * 
  * {{{
  * // default initial parameters
  * play { LinCongC.ar(MouseX.kr(20, SampleRate.ir)) * 0.2 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   LinCongC.ar(
  *     LFNoise2.kr(1.0).mulAdd(1e4, 1e4),
  *     LFNoise2.kr(0.1).mulAdd(0.5, 1.4),
  *     LFNoise2.kr(0.1).mulAdd(0.1, 0.1),
  *     LFNoise2.kr(0.1)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as frequency control
  * play {
  *   SinOsc.ar(
  *     LinCongC.ar(
  *       40,
  *       LFNoise2.kr(0.1).mulAdd(0.1, 1),
  *       LFNoise2.kr(0.1).mulAdd(0.1, 0.1),
  *       LFNoise2.kr(0.1)
  *     ).mulAdd(500, 600)
  *   ) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LinCongN$ LinCongN]]
  * @see [[de.sciss.synth.ugen.LinCongL$ LinCongL]]
  */
object LinCongC {
  def ar: LinCongC = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Multiplier amount
    * @param c                Increment amount
    * @param m                Modulus amount
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.1f, c: GE = 0.13f, m: GE = 1.0f, xi: GE = 0.0f): LinCongC = 
    new LinCongC(audio, freq, a, c, m, xi)
}

/** A cubic-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = (a * x[n] + c) % m
  * }}}
  *  The output signal is automatically scaled to a range of [-1, 1].
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Multiplier amount
  * @param c                Increment amount
  * @param m                Modulus amount
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.LinCongN$ LinCongN]]
  * @see [[de.sciss.synth.ugen.LinCongL$ LinCongL]]
  */
final case class LinCongC(rate: Rate, freq: GE = Nyquist(), a: GE = 1.1f, c: GE = 0.13f, m: GE = 1.0f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, c.expand, m.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A strange attractor discovered by Edward N. Lorenz while studying mathematical
  * models of the atmosphere. The system is composed of three ordinary differential
  * equations:
  * {{{
  * x' = s * (y - x)
  * y' = x * (r - z) - y
  * z' = x * y - b * z
  * }}}
  * The time step amount h determines the rate at which the ODE is evaluated.
  * Higher values will increase the rate, but cause more instability. A safe choice
  * is the default amount of 0.05.
  * 
  * ===Examples===
  * 
  * {{{
  * // vary frequency
  * play { LorenzL.ar(MouseX.kr(20, SampleRate.ir)) * 0.3 }
  * }}}
  * {{{
  * // randomly modulate parameters
  * play {
  *   LorenzL.ar(
  *     SampleRate.ir,
  *     LFNoise0.kr(1).mulAdd(2, 10),
  *     LFNoise0.kr(1).mulAdd(20, 38),
  *     LFNoise0.kr(1).mulAdd(1.5, 2)
  *   ) * 0.2
  * }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(Lag.ar(LorenzL.ar(MouseX.kr(1, 200)), 3e-3) * 800 + 900) * 0.4 }
  * }}}
  */
object LorenzL {
  def ar: LorenzL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param s                Equation variable
    * @param r                Equation variable
    * @param b                Equation variable
    * @param h                Integration time step
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    * @param zi               Initial value of z
    */
  def ar(freq: GE = Nyquist(), s: GE = 10.0f, r: GE = 28.0f, b: GE = 2.667f, h: GE = 0.05f, xi: GE = 0.1f, yi: GE = 0.0f, zi: GE = 0.0f): LorenzL = 
    new LorenzL(audio, freq, s, r, b, h, xi, yi, zi)
}

/** A strange attractor discovered by Edward N. Lorenz while studying mathematical
  * models of the atmosphere. The system is composed of three ordinary differential
  * equations:
  * {{{
  * x' = s * (y - x)
  * y' = x * (r - z) - y
  * z' = x * y - b * z
  * }}}
  * The time step amount h determines the rate at which the ODE is evaluated.
  * Higher values will increase the rate, but cause more instability. A safe choice
  * is the default amount of 0.05.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param s                Equation variable
  * @param r                Equation variable
  * @param b                Equation variable
  * @param h                Integration time step
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * @param zi               Initial value of z
  */
final case class LorenzL(rate: Rate, freq: GE = Nyquist(), s: GE = 10.0f, r: GE = 28.0f, b: GE = 2.667f, h: GE = 0.05f, xi: GE = 0.1f, yi: GE = 0.0f, zi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, s.expand, r.expand, b.expand, h.expand, xi.expand, yi.expand, zi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a * pow(x[n], 2) + b * x[n] + c
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // default parameters
  * play { QuadN.ar(SampleRate.ir/4) * 0.2 }
  * }}}
  * {{{
  * // logistic map
  * play {
  *   // equation: x1 = -r*x0^2 + r*x0
  *   val r = MouseX.kr(3.5441, 4)    // stable range
  *   QuadN.ar(SampleRate.ir/4, -r, r, 0, 0.1) * 0.4
  * }
  * }}}
  * {{{
  * // logistic map as frequency control
  * play {
  *   val r = MouseX.kr(3.5441, 4)    // stable range
  *   SinOsc.ar(QuadN.ar(40, -r, r, 0, 0.1).mulAdd(800, 900)) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.QuadL$ QuadL]]
  * @see [[de.sciss.synth.ugen.QuadC$ QuadC]]
  */
object QuadN {
  def ar: QuadN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param c                Equation variable
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = -1.0f, c: GE = -0.75f, xi: GE = 0.0f): QuadN = 
    new QuadN(audio, freq, a, b, c, xi)
}

/** A non-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a * pow(x[n], 2) + b * x[n] + c
  * }}}
  * 
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param c                Equation variable
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.QuadL$ QuadL]]
  * @see [[de.sciss.synth.ugen.QuadC$ QuadC]]
  */
final case class QuadN(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = -1.0f, c: GE = -0.75f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, c.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a * pow(x[n], 2) + b * x[n] + c
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // default parameters
  * play { QuadL.ar(SampleRate.ir/4) * 0.2 }
  * }}}
  * {{{
  * // logistic map
  * play {
  *   // equation: x1 = -r*x0^2 + r*x0
  *   val r = MouseX.kr(3.5441, 4)    // stable range
  *   QuadL.ar(SampleRate.ir/4, -r, r, 0, 0.1) * 0.4
  * }
  * }}}
  * {{{
  * // logistic map as frequency control
  * play {
  *   val r = MouseX.kr(3.5441, 4)    // stable range
  *   SinOsc.ar(QuadL.ar(40, -r, r, 0, 0.1).mulAdd(800, 900)) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.QuadN$ QuadN]]
  * @see [[de.sciss.synth.ugen.QuadC$ QuadC]]
  */
object QuadL {
  def ar: QuadL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param c                Equation variable
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = -1.0f, c: GE = -0.75f, xi: GE = 0.0f): QuadL = 
    new QuadL(audio, freq, a, b, c, xi)
}

/** A linear-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a * pow(x[n], 2) + b * x[n] + c
  * }}}
  * 
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param c                Equation variable
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.QuadN$ QuadN]]
  * @see [[de.sciss.synth.ugen.QuadC$ QuadC]]
  */
final case class QuadL(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = -1.0f, c: GE = -0.75f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, c.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A cubic-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a * pow(x[n], 2) + b * x[n] + c
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // default parameters
  * play { QuadC.ar(SampleRate.ir/4) * 0.2 }
  * }}}
  * {{{
  * // logistic map
  * play {
  *   // equation: x1 = -r*x0^2 + r*x0
  *   val r = MouseX.kr(3.5441, 4)    // stable range
  *   QuadC.ar(SampleRate.ir/4, -r, r, 0, 0.1) * 0.4
  * }
  * }}}
  * {{{
  * // logistic map as frequency control
  * play {
  *   val r = MouseX.kr(3.5441, 4)    // stable range
  *   SinOsc.ar(QuadC.ar(40, -r, r, 0, 0.1).mulAdd(800, 900)) * 0.4
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.QuadN$ QuadN]]
  * @see [[de.sciss.synth.ugen.QuadL$ QuadL]]
  */
object QuadC {
  def ar: QuadC = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param a                Equation variable
    * @param b                Equation variable
    * @param c                Equation variable
    * @param xi               Initial value of x
    */
  def ar(freq: GE = Nyquist(), a: GE = 1.0f, b: GE = -1.0f, c: GE = -0.75f, xi: GE = 0.0f): QuadC = 
    new QuadC(audio, freq, a, b, c, xi)
}

/** A cubic-interpolating sound generator based on the difference equation:
  * {{{
  * x[n+1] = a * pow(x[n], 2) + b * x[n] + c
  * }}}
  * 
  * 
  * @param freq             Iteration frequency in Hertz
  * @param a                Equation variable
  * @param b                Equation variable
  * @param c                Equation variable
  * @param xi               Initial value of x
  * 
  * @see [[de.sciss.synth.ugen.QuadN$ QuadN]]
  * @see [[de.sciss.synth.ugen.QuadL$ QuadL]]
  */
final case class QuadC(rate: Rate, freq: GE = Nyquist(), a: GE = 1.0f, b: GE = -1.0f, c: GE = -0.75f, xi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, a.expand, b.expand, c.expand, xi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = (x[n] + y[n+1]) % 2pi
  * y[n+1] = (y[n] + k * sin(x[n])) % 2pi
  * }}}
  * The standard map is an area preserving map of a cylinder discovered by the
  * plasma physicist Boris Chirikov.
  * 
  * ===Examples===
  * 
  * {{{
  * // vary frequency
  * play { StandardN.ar(MouseX.kr(20, SampleRate.ir)) * 0.3 }
  * }}}
  * {{{
  * // mouse-controlled parameter
  * play { StandardN.ar(SampleRate.ir/2, MouseX.kr(0.9, 4)) * 0.3 }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(StandardN.ar(40, MouseX.kr(0.9, 4)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.StandardL$ StandardL]]
  */
object StandardN {
  def ar: StandardN = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param k                Perturbation amount
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), k: GE = 1.0f, xi: GE = 0.5f, yi: GE = 0.0f): StandardN = 
    new StandardN(audio, freq, k, xi, yi)
}

/** A non-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = (x[n] + y[n+1]) % 2pi
  * y[n+1] = (y[n] + k * sin(x[n])) % 2pi
  * }}}
  * The standard map is an area preserving map of a cylinder discovered by the
  * plasma physicist Boris Chirikov.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param k                Perturbation amount
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.StandardL$ StandardL]]
  */
final case class StandardN(rate: Rate, freq: GE = Nyquist(), k: GE = 1.0f, xi: GE = 0.5f, yi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, k.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = (x[n] + y[n+1]) % 2pi
  * y[n+1] = (y[n] + k * sin(x[n])) % 2pi
  * }}}
  * The standard map is an area preserving map of a cylinder discovered by the
  * plasma physicist Boris Chirikov.
  * 
  * ===Examples===
  * 
  * {{{
  * // vary frequency
  * play { StandardL.ar(MouseX.kr(20, SampleRate.ir)) * 0.3 }
  * }}}
  * {{{
  * // mouse-controlled parameter
  * play { StandardL.ar(SampleRate.ir/2, MouseX.kr(0.9, 4)) * 0.3 }
  * }}}
  * {{{
  * // as a frequency control
  * play { SinOsc.ar(StandardL.ar(40, MouseX.kr(0.9, 4)) * 800 + 900) * 0.4 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.StandardN$ StandardN]]
  */
object StandardL {
  def ar: StandardL = ar()
  
  /** @param freq             Iteration frequency in Hertz
    * @param k                Perturbation amount
    * @param xi               Initial value of x
    * @param yi               Initial value of y
    */
  def ar(freq: GE = Nyquist(), k: GE = 1.0f, xi: GE = 0.5f, yi: GE = 0.0f): StandardL = 
    new StandardL(audio, freq, k, xi, yi)
}

/** A linear-interpolating sound generator based on the difference equations:
  * {{{
  * x[n+1] = (x[n] + y[n+1]) % 2pi
  * y[n+1] = (y[n] + k * sin(x[n])) % 2pi
  * }}}
  * The standard map is an area preserving map of a cylinder discovered by the
  * plasma physicist Boris Chirikov.
  * 
  * @param freq             Iteration frequency in Hertz
  * @param k                Perturbation amount
  * @param xi               Initial value of x
  * @param yi               Initial value of y
  * 
  * @see [[de.sciss.synth.ugen.StandardN$ StandardN]]
  */
final case class StandardL(rate: Rate, freq: GE = Nyquist(), k: GE = 1.0f, xi: GE = 0.5f, yi: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, k.expand, xi.expand, yi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}