/*
 *  GEOps.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

import de.sciss.synth.ugen.{BinaryOpUGen, ChannelProxy, ChannelRangeProxy, Clip, Constant, Flatten, Fold, Impulse, LinExp, LinLin, MulAdd, Poll, UnaryOpUGen, Wrap}

import scala.language.implicitConversions

object GEOps {
  private def getRate(g: GE, name: String): Rate =
    g.rate.getOrElse(throw new UnsupportedOperationException(s"`$name` input rate must be defined"))

  implicit def fromGE(g: GE): GEOps = new GEOps(g)
}

/** `GEOps` are operations for graph elements (`GE`). Instead of having these operations directly defined
  * in each UGen, which is a huge list, they appear here as extension methods. Therefore, you can
  * write `SinOsc.ar.linLin ...` as if `linLin` was defined for `GE` or `SinOsc`.
  *
  * Many of these operations are defined for constant numbers, as well, for example you can
  * write `0.5.linLin ...`. These operations are defined separately.
  *
  * @see [[GE]]
  */
final class GEOps(private val g: GE) extends AnyVal {
  import GEOps.getRate

  /** Creates a proxy that represents a specific output channel of the element.
    *
    * @param index  channel-index, zero-based. Indices which are greater than or equal
    *               to the number of outputs are wrapped around.
    *
    * @return a monophonic element that represents the given channel of the receiver
    *
    * @see [[ChannelProxy]]
    */
  def out(index: Int): GE = ChannelProxy(g, index)

  /** Creates a proxy that represents a specific range of output channels of the element.
    *
    * @param range  a range of channels, zero-based. Indices which are greater than or equal
    *               to the number of outputs are wrapped around.
    *
    * @return a new element that represents the given channels of the receiver
    *
    * @see [[ChannelRangeProxy]]
    */
  def out(range: Range): GE = {
    val until = if (range.isInclusive) range.end + range.step else range.end
    ChannelRangeProxy(g, from = range.start, until = until, step = range.step)
  }

  /** Creates a `MulAdd` UGen, by first multiplying the receiver with `mul`, then adding the `add` parameter.
    * It can be used to change the value range of the receiver. For example,
    * `SinOsc` has a nominal range of `-1` to `+1`. Using `SinOsc.ar.mulAdd(3, 4)` that
    * range would become `(-1 * 3) + 4 = 1` to `(+1 * 3) + 4 = 7`.
    *
    * @see [[MulAdd]]
    */
  def mulAdd(mul: GE, add: GE): GE = MulAdd(g, mul, add)

  /** Creates a `Flatten` graph element that "flattens" the channels from a nested multi-channel structure,
    * into a one-dimensional multi-channel structure.
    *
    * For example `Pan.ar(SinOsc.ar(Seq(400, 600)))` is a nested multi-channel structure, which appears
    * as two channels (nested) during expansion. Instead, `Pan.ar(SinOsc.ar(Seq(400, 600))).flatten` appears
    * as a flat four-channel signal.
    *
    * @see [[Flatten]]
    */
  def flatten: GE = Flatten(g)

  /** Creates a `Poll` UGen, printing the receiver's values to the console, ten times a second.
    *
    * @see [[Poll]]
    */
  def poll: Poll = poll()

  /** Polls the output values of this graph element, and prints the result to the console.
    * This is a convenient method for wrapping this graph element in a `Poll` UGen.
    *
    * @param   trig     a signal to trigger the printing. If this is a constant, it is
    *                   interpreted as a frequency value and an `Impulse` generator of that frequency
    *                   is used instead.
    * @param   label    a string to print along with the values, in order to identify
    *                   different polls. Using the special label `"\$auto"` (default) will generated
    *                   automatic useful labels using information from the polled graph element
    * @param   trigId   if greater then 0, a `"/tr"` OSC message is sent back to the client
    *                   (similar to `SendTrig`)
    *
    * @see  [[de.sciss.synth.ugen.Poll]]
    */
  def poll(trig: GE = 10, label: String = "$auto", trigId: GE = -1): Poll = {
    val trig1 = trig match {
      case Constant(freq) => Impulse((g.rate getOrElse audio) max control, freq, 0) // XXX good? or throw an error? should have a maxRate?
      case other          => other
    }
    val label1 = if (label != "$auto") label else {
      val str = g.toString
      val i   = str.indexOf('(')
      if (i >= 0) str.substring(0, i)
      else {
        val j = str.indexOf('@')
        if (j >= 0) str.substring(0, j)
        else str
      }
    }
    Poll(trig1.rate, trig = trig1, in = g, label = label1, trigId = trigId)
  }

  import UnaryOpUGen._

  @inline private def unOp(op: UnaryOpUGen.Op): GE = op.make(g)

  // unary ops

  /** Negatives the signal. As if multiplying the signal by `-1`.
    *
    * ===Example===
    *
    * {{{
    * // pseudo-stereo by phase inversion
    * play {
    *   val a = LFNoise1.ar(1500) * 0.5
    *   Seq(a, -a)
    * }
    * }}}
    */
  def unary_-   : GE  = unOp(Neg       )

  /** Logically negates the signal. Outputs `1` if the signal is greater than zero, otherwise outputs `0`. */
  def unary_!   : GE  = unOp(Not       )

  /** Treats the signal as integer numbers and inverts its bits. */
  def unary_~   : GE  = unOp(BitNot    )

  /** Takes the absolute values or magnitudes of the signal (negative numbers become positive).
    *
    * @see [[signum]]
    */
  def abs       : GE  = unOp(Abs       )
  // def toFloat: GE = ...
  // def toInteger: GE = ...

  /** Rounds the signal up to the next higher integer number.
    *
    * @see [[floor]]
    */
  def ceil      : GE  = unOp(Ceil      )

  /** Rounds the signal down to the next lower integer number.
    *
    * @see [[ceil]]
    */
  def floor     : GE  = unOp(Floor     )

  /** Takes the fractional part of the signal, equivalent to modulus `1.0`.
    *
    * @see  [[trunc]]
    */
  def frac      : GE  = unOp(Frac      )

  /** Takes the signum of the signal, being `1` for positive signals, `-1` for negative signals,
    * and zero for a zero input.
    *
    * @see [[abs]]
    */
  def signum    : GE  = unOp(Signum    )

  /** Takes the square of the signal, equivalent to `a * a`.
    *
    * @see [[pow]]
    */
  def squared   : GE  = unOp(Squared   )

  /** Takes the signal to the power of three, equivalent to `a * a * a`.
    *
    * @see [[pow]]
    */
  def cubed     : GE  = unOp(Cubed     )

  /** Takes the square root of the signal. If the input is negative, it returns
    * the negative of the square root of the absolute value, i.e. `sqrt(-2) == -sqrt(2)`,
    * so the output is valid for any input.
    *
    * @see [[pow]]
    */
  def sqrt      : GE  = unOp(Sqrt      )

  /** Exponentiates the signal (with base ''e'').
    *
    * The inverse function is `log`.
    *
    * @see  [[log]]
    */
  def exp       : GE  = unOp(Exp       )

  /** Takes the reciprocal value of the input signal, equivalent to `1.0 / a`.
    *
    * ''Warning:'' Outputs `NaN` if the input is zero ("division by zero").
    */
  def reciprocal: GE  = unOp(Reciprocal)

  /** Converts the input signal from midi pitch to a frequency in cycles per second (Hertz).
    * A midi pitch of `69` corresponds to A4 or 440 cps. A pitch of `69 + 12 = 81` is one
    * octave up, i.e. A5 or 880 cps.
    *
    * The inverse function is `cpsMidi`.
    *
    * ===Example===
    *
    * {{{
    * // midi pitch from 24 to 108 to oscillator frequency
    * play {
    *   Saw.ar(Line.kr(24, 108, dur = 10).midiCps) * 0.2
    * }
    * }}}
    *
    * @see [[cpsMidi]]
    */
  def midiCps   : GE  = unOp(Midicps   )

  /** Converts the input signal from a frequency in cycles per second (Hertz) to a midi pitch number.
    * A frequency of 440 cps corresponds to the midi pitch of `69` or note A4.
    *
    * The inverse function is `midiCps`.
    *
    * @see [[midiCps]]
    */
  def cpsMidi   : GE  = unOp(Cpsmidi   )

  /** Converts the input signal from midi pitch interval to a frequency ratio.
    * An interval of zero corresponds to unison and thus a frequency ratio of `1.0`.
    * An interval of `+12` corresponds to an octave up or ratio of `2.0`.
    * An interval of `-12` corresponds to an octave down or ratio of `(1.0/2.0) = 0.5`.
    *
    * The inverse function is `ratioMidi`.
    *
    * @see [[ratioMidi]]
    */
  def midiRatio : GE  = unOp(Midiratio )

  /** Converts the input signal from a frequency ratio to a midi pitch interval.
    * A frequency ratio of 1.0 corresponds to the unison or midi interval 0.
    * A ratio of 2.0 corresponds to an octave up or the midi pitch interval `+12`.
    * A ratio of 0.5 corresponds to an octave down or the midi pitch interval `-12`.
    *
    * The inverse function is `midiRatio`.
    *
    * @see [[midiRatio]]
    */
  def ratioMidi : GE  = unOp(Ratiomidi )

  /** Converts the input signal from a level in decibels (dB) to a linear factor.
    * A level of 0 dB corresponds to a factor of 1.0. A level of +6 dB corresponds
    * to a factor of c. 2.0 (double amplitude).
    * A level of -6 dB corresponds to a factor of c. 0.5 (half amplitude).
    *
    * The inverse function is `ampDb`.
    *
    * @see  [[ampDb]]
    */
  def dbAmp     : GE  = unOp(Dbamp     )

  /** Converts the input signal from a linear (amplitude) factor to a level in decibels (dB).
    * A unit amplitude of 1.0 corresponds with 0 dB. A factor or amplitude of 2.0 corresponds
    * to c. +6 dB. A factor or amplitude of 0.5 corresponds wot c. -6 dB.
    *
    * The inverse function is `dbAmp`.
    *
    * @see  [[dbAmp]]
    */
  def ampDb     : GE  = unOp(Ampdb     )

  /** Converts the input signal from "decimal octaves" to a frequency in cycles per second (Hertz).
    * For example, octave 4 begins with 4.0 or note C4, corresponding to 261.626 cps. The tritone
    * above that (plus 6 semitones or half an octave), is "decimal octave" 4.5, corresponding with
    * note F♯4 or 369.994 cps.
    *
    * The inverse function is `cpsOct`.
    *
    * @see  [[cpsOct]]
    */
  def octCps    : GE  = unOp(Octcps    )

  /** Converts the input signal from a frequency in cycles per second (Hertz) to a "decimal octave".
    * A frequency of 261.626 cps corresponds to the note C4 or "decimal octave" 4.0.
    *
    * The inverse function is `cpsOct`.
    *
    * @see [[cpsOct]]
    */
  def cpsOct    : GE  = unOp(Cpsoct    )

  /** Takes the natural logarithm of the input signal.
    *
    * ''Warning:'' Outputs `NaN` if the input is negative, and `-Infinity` if the input is zero.
    *
    * The inverse function is `exp`.
    *
    * @see  [[exp]]
    * @see  [[log2]]
    * @see  [[log10]]
    */
  def log       : GE  = unOp(Log       )

  /** Takes the logarithm of the input signal, using base 2.
    *
    * ''Warning:'' Outputs `NaN` if the input is negative, and `-Infinity` if the input is zero.
    *
    * @see  [[log]]
    * @see  [[log10]]
    */
  def log2      : GE  = unOp(Log2      )

  /** Takes the logarithm of the input signal, using base 10.
    *
    * ''Warning:'' Outputs `NaN` if the input is negative, and `-Infinity` if the input is zero.
    *
    * @see  [[log]]
    * @see  [[log2]]
    */
  def log10     : GE  = unOp(Log10     )

  /** Uses the input signal as argument to the sine (trigonometric) function. */
  def sin       : GE  = unOp(Sin       )

  /** Uses the input signal as argument to the cosine (trigonometric) function. */
  def cos       : GE  = unOp(Cos       )

  /** Uses the input signal as argument to the tangent (trigonometric) function. */
  def tan       : GE  = unOp(Tan       )

  /** Uses the input signal as argument to the arc sine (trigonometric) function.
    * Warning: produces `NaN` if the input is less than `-1` or greater than `+1`
    */
  def asin      : GE  = unOp(Asin      )

  /** Uses the input signal as argument to the arc cosine (trigonometric) function.
    * Warning: produces `NaN` if the input is less than `-1` or greater than `+1`
    */
  def acos      : GE  = unOp(Acos      )

  /** Uses the input signal as argument to the arc tangent (trigonometric) function.
    * It is also useful for "compressing" the range of an input signal.
    *
    * There is also a binary operator `atan2` for a greater (disambiguated) value range.
    *
    * @see [[atan2]]
    */
  def atan      : GE  = unOp(Atan      )

  /** Uses the input signal as argument to the hyperbolic sine (trigonometric) function.
    */
  def sinh      : GE  = unOp(Sinh      )

  /** Uses the input signal as argument to the hyperbolic cosine (trigonometric) function.
    */
  def cosh      : GE  = unOp(Cosh      )

  /** Uses the input signal as argument to the hyperbolic tangent (trigonometric) function.
    */
  def tanh      : GE  = unOp(Tanh      )

  /** Produces a random signal evenly distributed between zero and the input signal.
    *
    * @see  [[linRand]]
    */
  def rand      : GE  = unOp(Rand      )

  /** Produces a random signal evenly distributed between `-a` and `+a` for input signal `a`.
    *
    * @see  [[bilinRand]]
    */
  def rand2     : GE  = unOp(Rand2     )

  /** Produces a random signal linearly distributed between zero and the input signal.
    *
    * @see  [[rand]]
    */
  def linRand   : GE  = unOp(Linrand   )

  /** Produces a random signal linearly distributed between `-a` and `+a` for input signal `a`.
    *
    * @see  [[rand2]]
    */
  def bilinRand : GE  = unOp(Bilinrand )

  /** Produces a random signal following an approximated Gaussian distribution between zero and the input signal.
    * It follows the formula `Mix.fill(3)(Rand(0.0, 1.0)) - 1.5 * (2.0/3)`,
    * thus summing three evenly distributed signals
    */
  def sum3Rand  : GE  = unOp(Sum3rand  )

  /** Produces a non-linear distortion using the formula `a / (1 + abs(a))`.
    *
    * ===Example===
    *
    * {{{
    * // gradually increasing amount of distortion
    * play {
    *   (SinOsc.ar(500) * XLine.kr(0.1, 10, 10)).distort * 0.25
    * }
    * }}}
    *
    * @see  [[softClip]]
    */
  def distort   : GE  = unOp(Distort   )

  /** Produces a non-linear distortion, wherein the input range of `-0.5` to `+0.5` is linear.
    *
    * ===Example===
    *
    * {{{
    * // gradually increasing amount of distortion
    * play {
    *   (SinOsc.ar(500) * XLine.kr(0.1, 10, 10)).softClip * 0.25
    * }
    * }}}
    *
    * @see  [[distort]]
    */
  def softClip  : GE  = unOp(Softclip  )

  /** Produces a random signal taking the values zero or one, using the input
    * signal as probability. If the input is `0.5`, there is a 50:50 chance of the output
    * sample becoming zero or one. If the input is `0.1`, the chance of an output sample
    * becoming zero is 90% and the chance of it becoming one is 10%.
    *
    * If the input is zero or less than zero, the output is constantly zero. If the input
    * is one or greater than one, the output is constantly one.
    */
  def coin      : GE  = unOp(Coin      )

  // def even : GE              = UnOp.make( 'even, this )
  // def odd : GE               = UnOp.make( 'odd, this )

  /** Scans the rectangular window function between the input signal's range of 0 and 1.
    * A rectangular window has value 1.0 at every point in the interval `[0, 1]`.
    * Outside of that range, the output will be zero.
    */
  def rectWindow : GE  = unOp(RectWindow )

  /** Scans the von Hann window function between the input signal's range of 0 and 1.
    * The Hann window is a raised cosine starting and ending at zero at the interval
    * boundaries, and going to 1.0 at the center (input 0.5).
    * Outside of the interval `[0, 1]`, the output will be zero.
    */
  def hannWindow : GE  = unOp(HannWindow )

  /** Scans the Welch window function between the input signal's range of 0 and 1.
    * The Welch window is a parabolic curve starting and ending at zero at the interval
    * boundaries, and going to 1.0 at the center (input 0.5).
    * Outside of the interval `[0, 1]`, the output will be zero.
    */
  def welchWindow: GE  = unOp(WelchWindow)

  /** Scans the triangular window function between the input signal's range of 0 and 1.
    * The triangular window moves from linearly from the zero at the interval start to
    * 1.0 at the center (input 0.5) back to 0.0 at the interval end.
    * Outside of the interval `[0, 1]`, the output will be zero.
    */
  def triWindow  : GE  = unOp(TriWindow  )

  /** Scans a ramp function rising from zero to one across the input signals' range of 0 and 1.
    * The function moves linearly from the zero at the interval start to
    * 1.0 at the interval end. When the input is smaller than zero, the output is zero,
    * and when the input is larger than one, the output is one.
    */
  def ramp       : GE  = unOp(Ramp       )

  /** Scans a sinusoidal S-shaped function ("easy in, easy out") rising from zero to one across
    * the input signals' range of 0 and 1.
    * The function begins to rise from the zero at the interval start to 0.5 at the center to
    * 1.0 at the interval end. When the input is smaller than zero, the output is zero,
    * and when the input is larger than one, the output is one.
    */
  def sCurve     : GE  = unOp(Scurve     )

  // def isPositive : GE        = UnOp.make( 'isPositive, this )
  // def isNegative : GE        = UnOp.make( 'isNegative, this )
  // def isStrictlyPositive : GE= UnOp.make( 'isStrictlyPositive, this )
  // def rho : GE               = UnOp.make( 'rho, this )
  // def theta : GE             = UnOp.make( 'theta, this )

  import BinaryOpUGen._

  // binary ops
  @inline private def binOp(op: BinaryOpUGen.Op, b: GE): GE = op.make(g, b)

  /** Adds two signals. */
  def +       (b: GE): GE = binOp(Plus    , b)

  /** Subtracts a signal from the receiver. */
  def -       (b: GE): GE = binOp(Minus   , b)

  /** Multiplies two signals. */
  def *       (b: GE): GE = binOp(Times   , b)
  // def div(b: GE): GE = ...

  /** Divides the receiver by another signal. */
  def /       (b: GE): GE = binOp(Div     , b)

  /** Take the modulus of the signal. Negative input values are
    * wrapped to positive ones, e.g. `(DC.kr(-4) % 5) sig_== DC.kr(1)`.
    * If the second operand is zero, the output is zero.
    *
    * An alias for mod.
    */
  def %       (b: GE): GE = binOp(Mod     , b)

  /** Take the modulus of the signal. Negative input values are
    * wrapped to positive ones, e.g. `DC.kr(-4).mod(5) sig_== DC.kr(1)`
    * If the second operand is zero, the output is zero.
    */
  def mod     (b: GE): GE = binOp(Mod     , b)

  /** Compares two signals and outputs one if they are identical, otherwise zero.
    *
    * Note that this can be surprising if the signals are not integer, because
    * due to floating point noise two signals may be "almost identical" but not quite,
    * thus resulting in an output of zero.
    */
  def sig_==  (b: GE): GE = binOp(Eq      , b)

  /** Compares two signals and outputs one if they are different, otherwise zero.
    *
    * Note that this can be surprising if the signals are not integer, because
    * due to floating point noise two signals may be "almost identical" but not quite,
    * thus resulting in an output of one.
    */
  def sig_!=  (b: GE): GE = binOp(Neq     , b)

  /** Compares two signals and outputs one if the receiver is less than the argument. */
  def <       (b: GE): GE = binOp(Lt      , b)

  /** Compares two signals and outputs one if the receiver is greater than the argument.
    *
    * ===Example===
    *
    * {{{
    * // trigger an envelope
    * play {
    *   val trig = SinOsc.ar(1) > 0
    *   val env = EnvGen.kr(Env.perc, gate = trig, doneAction = doNothing) *
    *   SinOsc.ar(440) * env * 0.1
    * }
    * }}}
    */
  def >       (b: GE): GE = binOp(Gt      , b)

  /** Compares two signals and outputs one if the receiver is less than or equal to the argument.
    *
    * Note that this can be surprising if the signals are not integer, because
    * due to floating point noise two signals may be "almost identical" but not quite.
    */
  def <=      (b: GE): GE = binOp(Leq     , b)

  /** Compares two signals and outputs one if the receiver is greater than or equal to the argument.
    *
    * Note that this can be surprising if the signals are not integer, because
    * due to floating point noise two signals may be "almost identical" but not quite.
    */
  def >=      (b: GE): GE = binOp(Geq     , b)

  /** Outputs the smaller of two signals.
    *
    * ===Example===
    *
    * {{{
    * // LFO distorts and envelopes z
    * play {
    *   val z = SinOsc.ar(500)
    *   z min SinOsc.ar(0.1)
    * }
    * }}}
    */
  def min     (b: GE): GE = binOp(Min     , b)

  /** Outputs the larger of two signals.
    *
    * ===Example===
    *
    * {{{
    * // LFO distorts and envelopes z
    * play {
    *   val z = SinOsc.ar(500)
    *   z max SinOsc.ar(0.1)
    * }
    * }}}
    */
  def max     (b: GE): GE = binOp(Max     , b)

  /** Treats the signals as integer numbers and combines their bit representations through `AND`. */
  def &       (b: GE): GE = binOp(BitAnd  , b)

  /** Treats the signals as integer numbers and combines their bit representations through `OR`. */
  def |       (b: GE): GE = binOp(BitOr   , b)

  /** Treats the signals as integer numbers and combines their bit representations through `XOR`. */
  def ^       (b: GE): GE = binOp(BitXor  , b)

  /** Treats the signals as integer numbers and outputs the least common multiple of both.
    * This extends the usual definition by returning a negative number if any of the operands is negative.
    *
    * ===Example===
    *
    * {{{
    * // rhythmic pitch patterns with mouse control
    * play {
    *   val mx = MouseX.kr(-20, 20)
    *   val my = MouseY.kr(-20, 20)
    *   SinOsc.ar((SinOsc.kr(0.3) * 20) lcm (GESeq(mx, my) * 30 + 500)) * 0.1
    * }
    * }}}
    *
    * @see  [[gcd]]
    */
  def lcm     (b: GE): GE = binOp(Lcm     , b)

  /** Treats the signals as integer numbers and outputs the greatest common divisor of both.
    * This extends the usual definition by returning a negative number if both operands are negative.
    * "greater" means "divisible by" in this interpretation, so `-1 gcd -1` returns a negative number.
    *
    * ===Example===
    *
    * {{{
    * // rhythmic pitch patterns with mouse control
    * play {
    *   val mx = MouseX.kr(-200, 200)
    *   val my = MouseY.kr(-200, 200)
    *   val freq = (SinOsc.kr(0.3) * 20).gcd(GESeq(mx, my)) * 30 + 500
    *   SinOsc.ar(freq) * 0.1
    * }
    * }}}
    *
    * @see  [[lcm]]
    */
  def gcd     (b: GE): GE = binOp(Gcd     , b)

  /** Rounds the input signal up or down to a given degree of coarseness. For example,
    * `roundTo(1.0)` rounds to the closest integer, `roundTo(0.1)` rounds to the closest
    * number for which modulus 0.1 is zero.
    */
  def roundTo (b: GE): GE = binOp(RoundTo , b)

  /** Rounds the input signal up to a given degree of coarseness. For example,
    * `roundUpTo(1.0)` rounds up to the closest integer, `roundTo(0.1)` rounds up to the closest
    * number for which modulus 0.1 is zero.
    */
  def roundUpTo (b: GE): GE = binOp(RoundUpTo, b)

  /** Removes the fractional part of the input signal.
    *
    * @see  [[frac]]
    */
  def trunc   (b: GE): GE = binOp(Trunc   , b)

  /** Uses the input signal and the argument to calculate the arc tangent (trigonometric) function.
    * The input signal is the numerator ("x") and the argument `b` is the denominator ("y") of the parameter
    * to arg tangent.
    *
    * There is also a unary operator `atan`.
    *
    * @see [[atan]]
    */
  def atan2   (b: GE): GE = binOp(Atan2   , b)

  /** Calculates the hypotenuse of both signals, or the square root of the sum of the squares of both.
    *
    * ===Example===
    *
    * {{{
    * // hypot used to calculate a Doppler shift pitch and amplitude based on distance.
    * play {
    *   // object travels 200 meters in 6 secs (= 120 km/h) passing 10 meters
    *   // from the listener
    *   val x = 10
    *   val y = LFSaw.kr(1.0/6) * 100
    *   val distance = x hypot y
    *   val velocity = Slope.kr(distance)
    *   val pitchRatio = (344 - velocity) / 344  // speed of sound is around 344 m/s
    *   val amplitude = 20 / distance.squared
    *   SinOsc.ar(500 * pitchRatio) * amplitude
    * }
    * }}}
    *
    * @see [[hypotApx]]
    */
  def hypot   (b: GE): GE = binOp(Hypot   , b)

  /** An approximate and thus faster version of `hypot` to calculate the hypotenuse of both signals,
    * or the square root of the sum of the squares of both.
    *
    * @see [[hypot]]
    */
  def hypotApx(b: GE): GE = binOp(Hypotx  , b)

  /** Takes the power of the signal.
    *
    * '''Warning:''' Unlike a normal power operation, the signum of the
    * left operand is always preserved. I.e. `DC.kr(-0.5).pow(2)` will
    * not output `0.25` but `-0.25`. This is to avoid problems with
    * floating point noise and negative input numbers, so
    * `DC.kr(-0.5).pow(2.001)` does not result in a `NaN`, for example.
    */
  def pow     (b: GE): GE = binOp(Pow     , b)

  /** Treats the signals as integer numbers and bit-shifts the receiver by the argument to the left. */
  def <<      (b: GE): GE = binOp(LeftShift , b)

  /** Treats the signals as integer numbers and bit-shifts the receiver by the argument to the right. */
  def >>      (b: GE): GE = binOp(RightShift, b)

  // def unsgnRghtShift(b: GE): GE = ...
  // def fill(b: GE): GE = ...

  /** An optimized operation on the signals corresponding to the formula `a * b + a`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) ring1 SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def ring1   (b: GE): GE = binOp(Ring1   , b)

  /** An optimized operation on the signals corresponding to the formula `a * b + a + b`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) ring2 SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def ring2   (b: GE): GE = binOp(Ring2   , b)

  /** An optimized operation on the signals corresponding to the formula `a * a * b`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) ring3 SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def ring3   (b: GE): GE = binOp(Ring3   , b)

  /** An optimized operation on the signals corresponding to the formula `a * a * b - b * b * a`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) ring4 SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def ring4   (b: GE): GE = binOp(Ring4   , b)

  /** Calculates the difference of the squared arguments, equivalent to the formula `(a*a) - (b*b)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) difSqr SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def difSqr  (b: GE): GE = binOp(Difsqr  , b)

  /** Calculates the sum of the squared arguments, equivalent to the formula `(a*a) + (b*b)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) sumSqr SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def sumSqr  (b: GE): GE = binOp(Sumsqr  , b)

  /** Squares the sum of the two arguments, equivalent to the formula `(a + b) * (a + b)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) sqrSum SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def sqrSum  (b: GE): GE = binOp(Sqrsum  , b)

  /** Squares the difference of the two arguments, equivalent to the formula `(a - b) * (a - b)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   (SinOsc.ar(800) sqrDif SinOsc.ar(XLine.kr(200, 500, 5))) * 0.125
    * }
    * }}}
    */
  def sqrDif  (b: GE): GE = binOp(Sqrdif  , b)

  /** Calculates the absolute difference of the two arguments, equivalent to the formula `abs(a - b)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   // creates a pulsation
    *   val mul = (SinOsc.ar(2) * 0.5) absDif 0.2
    *   SinOsc.ar(440) * mul
    * }
    * }}}
    */
  def absDif  (b: GE): GE = binOp(Absdif  , b)

  /** A threshold comparison where the output is zero when input signal is less than the argument, otherwise
    * the input signal is passed.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   // a low-rent gate
    *   (LFNoise0.ar(50) * 0.5) thresh 0.45
    * }
    * }}}
    *
    * @see  [[de.sciss.synth.ugen.Gate]]
    */
  def thresh  (b: GE): GE = binOp(Thresh  , b)

  /** Multiplies the input signal with the argument, while the argument
    * is positive, otherwise outputs zero. Thus equivalent to `a * b.max(0)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   // a sine envelope
    *   WhiteNoise.ar.amClip(SinOsc.kr(1) * 0.2)
    * }
    * }}}
    */
  def amClip  (b: GE): GE = binOp(Amclip  , b)

  /** Scales the negative part of the input by the argument, outputs positive input unaltered.
    * Thus equivalent to `a.min(0) * b + a.max(0)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   // increasing half-wave asymmetry
    *   (SinOsc.ar(500) * 0.25).scaleNeg(Line.ar(1, -1, 4))
    * }
    * }}}
    */
  def scaleNeg(b: GE): GE = binOp(Scaleneg, b)

  /** Clips the input signal to the range given by +/- the argument.
    * If the input falls below `-b`, it is held at `-b`.
    * If the input rises above `+b`, it is held at `+b`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   // fade in amplitude, fade out distortion
    *   SinOsc.ar(500).clip2(Line.kr(0, 1, 8)) * 0.5
    * }
    * }}}
    *
    * @see [[clip]]
    */
  def clip2   (b: GE): GE = binOp(Clip2   , b)

  /** Produces the difference of the input signal and it being clipped to the argument.
    * Thus equivalent to `a - a.clip2(b)`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   SinOsc.ar(500).excess(Line.kr(0, 1, 8)) * 0.5
    * }
    * }}}
    */
  def excess  (b: GE): GE = binOp(Excess  , b)

  /** Folds or "reflects" the input signal at the boundaries given by +/- the argument.
    * For example, with boundaries of +/- 2,
    * if the input falls below -2, it is reflected such that input -2.1 becomes output -1.9.
    * If the input rises above +2, it is reflected such that input +2.1 becomes output +1.9.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   SinOsc.ar(500).fold2(Line.kr(0, 1, 8)) * 0.5
    * }
    * }}}
    *
    * @see  [[clip2]]
    * @see  [[wrap2]]
    * @see  [[fold]]
    */
  def fold2   (b: GE): GE = binOp(Fold2   , b)

  /** Wraps the input signal at the boundaries given by +/- the argument.
    * If the input falls below `-b`, it is wrapped back to `+b`.
    * If the input rises above `+b`, it is wrapped back to `-b`.
    *
    * ===Example===
    *
    * {{{
    * play {
    *   SinOsc.ar(500).wrap2(Line.kr(0, 1, 8)) * 0.5
    * }
    * }}}
    *
    * @see  [[clip2]]
    * @see  [[fold2]]
    * @see  [[wrap]]
    */
  def wrap2   (b: GE): GE = binOp(Wrap2   , b)

  /** A dummy operation that ensures the topological order of the receiver UGen and the argument UGen.
    * It ensures that the receiver is placed before the argument. Rarely used.
    *
    * ===Example===
    *
    * {{{
    * // useful when two UGens need to be called, but only one of their outputs is used
    * play {
    *   val a = Dpoll(Dseq(Seq(1, 2, 3, 4), inf), label = "a")
    *   val b = Dpoll(Dseq(Seq(1955, 1952, 1823, 1452), inf), label = "b")
    *   val c = Dpoll(a firstArg b, label = "a firstArg b ")  // c = a
    *   Duty.kr(0.4, 0, c)
    *   ()
    * }
    * }}}
    */
  def firstArg(b: GE): GE = binOp(Firstarg, b)

  /** Outputs random values evenly distributed between the two signals. */
  def rangeRand(b: GE): GE = binOp(Rrand   , b)

  /** Outputs random values exponentially distributed between the two signals. */
  def expRand  (b: GE): GE = binOp(Exprand , b)

  /** Clips the receiver to a range defined by `low` and `high`.
    * Outputs `low` if the receiver is smaller than `low`, and
    * outputs `high` if the receiver is larger than `high`.
    *
    * @see  [[fold]]
    * @see  [[wrap]]
    * @see  [[clip2]]
    */
  def clip(low: GE, high: GE): GE = {
    import GEOps.{fromGE => geOps}  // N.B. renamed to shadow ScalaCollider's package object's `geOps` (unidoc!)
    val r = getRate(g, "clip")
    if (r == demand) g.max(low).min(high) else Clip(r, g, low, high)
  }

  /** Folds or "reflects" the input signal at the boundaries given by the arguments.
    * For example, with `low = -2` and `high = +3`
    * if the input falls below -2, it is reflected such that input -2.1 becomes output -1.9.
    * If the input rises above +3, it is reflected such that input +3.1 becomes output +2.9.
    *
    * @see  [[clip]]
    * @see  [[wrap]]
    * @see  [[fold2]]
    */
  def fold(low: GE, high: GE): GE = {
    val r = getRate(g, "fold")
    if (r == demand) throw new UnsupportedOperationException("`fold` not supported for demand rate UGens")
    Fold(r, g, low, high)
  }

  /** Wraps the input signal at the boundaries given by the arguments.
    * If the input falls below `low`, it is wrapped back to `high`.
    * If the input rises above `high`, it is wrapped back to `low`.
    *
    * @see  [[clip]]
    * @see  [[fold]]
    * @see  [[wrap2]]
    */
  def wrap(low: GE, high: GE): GE = {
    val r = getRate(g, "wrap")
    if (r == demand) throw new UnsupportedOperationException("`wrap` not supported for demand rate UGens")
    Wrap(r, g, low, high)
  }

  /** Scales the input signal `a` from a linear source range to a linear target range.
    * The formula is `(a - inLow) / (inHigh - inLow) * (outHigh - outLow) + outLow`.
    */
  def linLin(inLow: GE, inHigh: GE, outLow: GE, outHigh: GE): GE = {
    import GEOps.{fromGE => geOps}
    val r = getRate(g, "linLin")
    if (r == demand) {
      (g - inLow) / (inHigh - inLow) * (outHigh - outLow) + outLow
    } else {
      LinLin(/* rate, */ g, inLow, inHigh, outLow, outHigh)
    }
  }

  /** Scales the input signal `a` from a linear source range to an exponential target range.
    * The formula is `(outHigh / outLow).pow((a - inLow) / (inHigh - inLow)) * outLow`.
    */
  def linExp(inLow: GE, inHigh: GE, outLow: GE, outHigh: GE): GE = {
    import GEOps.{fromGE => geOps}
    val r = getRate(g, "linExp")
    if (r == demand) {
      (outHigh / outLow).pow((g - inLow) / (inHigh - inLow)) * outLow
    } else {
      LinExp(g.rate, g, inLow, inHigh, outLow, outHigh) // should be highest rate of all inputs? XXX
    }
  }

  /** Scales the input signal `a` from an exponential source range to a linear target range.
    * The formula is `(a / inLow).log / (inHigh / inLow).log * (outHigh - outLow) + outLow`.
    */
  def expLin(inLow: GE, inHigh: GE, outLow: GE, outHigh: GE): GE = {
    import GEOps.{fromGE => geOps}
    (g / inLow).log / (inHigh / inLow).log * (outHigh - outLow) + outLow
  }

  /** Scales the input signal `a` from an exponential source range to an exponential target range.
    * The formula is `(outHigh / outLow).pow((a / inLow).log / (inHigh / inLow).log) * outLow`.
    */
  def expExp(inLow: GE, inHigh: GE, outLow: GE, outHigh: GE): GE = {
    import GEOps.{fromGE => geOps}
    (outHigh / outLow).pow((g / inLow).log / (inHigh / inLow).log) * outLow
  }
}