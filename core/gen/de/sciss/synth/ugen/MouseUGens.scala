// revision: 2
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that maps the horizontal screen location of the mouse to a given linear
  * or exponential range. This is useful for testing purposes. Mouse interaction
  * with the regular desktop and windowing system is in no way altered by running
  * this UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // control sine frequency
  * play {
  *   val freq = MouseX.kr(100, 4000, 1)
  *   SinOsc.ar(freq) * 0.1 * AmpComp.kr(freq)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.MouseY$ MouseY]]
  * @see [[de.sciss.synth.ugen.MouseButton$ MouseButton]]
  * @see [[de.sciss.synth.ugen.KeyState$ KeyState]]
  */
object MouseX extends ProductReader[MouseX] {
  def kr: MouseX = kr()
  
  /** @param lo               value when the mouse is on the left side of the screen
    * @param hi               value when the mouse is on the right side of the
    *                         screen. '''Note''': this value is never reached, because
    *                         the maximum mouse coordinate is one pixel less than the
    *                         screen size. For example, if the screen width is 1440,
    *                         `lo` is 0.0 and `hi` is 1.0, the maximum value output is
    *                         `1.0 * 1439/1440 = 0.999306` .
    * @param warp             curve shape. Either zero (default) for a linear
    *                         mapping, or 1 for an exponential mapping. '''Note''':
    *                         When using exponential mapping, make sure the `lo` value
    *                         is greater than zero, otherwise NaN values will be
    *                         output.
    * @param lag              60 dB decay time of a lag-time smoothing. Use zero to
    *                         avoid any smoothing.
    */
  def kr(lo: GE = 0.0f, hi: GE = 1.0f, warp: GE = 0.0f, lag: GE = 0.2f): MouseX = 
    new MouseX(lo, hi, warp, lag)
  
  def read(in: RefMapIn, key: String, arity: Int): MouseX = {
    require (arity == 4)
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    val _warp = in.readGE()
    val _lag  = in.readGE()
    new MouseX(_lo, _hi, _warp, _lag)
  }
}

/** A UGen that maps the horizontal screen location of the mouse to a given linear
  * or exponential range. This is useful for testing purposes. Mouse interaction
  * with the regular desktop and windowing system is in no way altered by running
  * this UGen.
  * 
  * @param lo               value when the mouse is on the left side of the screen
  * @param hi               value when the mouse is on the right side of the
  *                         screen. '''Note''': this value is never reached, because
  *                         the maximum mouse coordinate is one pixel less than the
  *                         screen size. For example, if the screen width is 1440,
  *                         `lo` is 0.0 and `hi` is 1.0, the maximum value output is
  *                         `1.0 * 1439/1440 = 0.999306` .
  * @param warp             curve shape. Either zero (default) for a linear
  *                         mapping, or 1 for an exponential mapping. '''Note''':
  *                         When using exponential mapping, make sure the `lo` value
  *                         is greater than zero, otherwise NaN values will be
  *                         output.
  * @param lag              60 dB decay time of a lag-time smoothing. Use zero to
  *                         avoid any smoothing.
  * 
  * @see [[de.sciss.synth.ugen.MouseY$ MouseY]]
  * @see [[de.sciss.synth.ugen.MouseButton$ MouseButton]]
  * @see [[de.sciss.synth.ugen.KeyState$ KeyState]]
  */
final case class MouseX(lo: GE = 0.0f, hi: GE = 1.0f, warp: GE = 0.0f, lag: GE = 0.2f)
  extends UGenSource.SingleOut with ControlRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, warp.expand, lag.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, control, _args)
}

/** A UGen that maps the vertical screen location of the mouse to a given linear or
  * exponential range. The `lo` value corresponds to the ''bottom'' of the screen,
  * and the `hi` value corresponds to the ''top'' of the screen (not vice-versa).
  * 
  * This UGen is useful for testing purposes. Mouse interaction with the regular
  * desktop and windowing system is in no way altered by running this UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // control sine frequency
  * play {
  *   val freq = MouseY.kr(100, 4000, 1)
  *   SinOsc.ar(freq) * 0.1 * AmpComp.kr(freq)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.MouseX$ MouseX]]
  * @see [[de.sciss.synth.ugen.MouseButton$ MouseButton]]
  * @see [[de.sciss.synth.ugen.KeyState$ KeyState]]
  */
object MouseY extends ProductReader[MouseY] {
  def kr: MouseY = kr()
  
  /** @param lo               value when the mouse is on the bottom side of the
    *                         screen. '''Note''': this value is never reached, because
    *                         the maximum mouse coordinate is one pixel less than the
    *                         screen height. For example, if the screen height is 900,
    *                         `lo` is 0.0 and `hi` is 1.0, the minimum value output is
    *                         `1.0 - 1.0 * 899.0/900 = 0.001111` .
    * @param hi               value when the mouse is on the top side of the screen
    * @param warp             curve shape. Either zero (default) for a linear
    *                         mapping, or 1 for an exponential mapping. '''Note''':
    *                         When using exponential mapping, make sure the `lo` value
    *                         is greater than zero, otherwise NaN values will be
    *                         output.
    * @param lag              60 dB decay time of a lag-time smoothing. Use zero to
    *                         avoid any smoothing.
    */
  def kr(lo: GE = 0.0f, hi: GE = 1.0f, warp: GE = 0.0f, lag: GE = 0.2f): MouseY = 
    new MouseY(lo, hi, warp, lag)
  
  def read(in: RefMapIn, key: String, arity: Int): MouseY = {
    require (arity == 4)
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    val _warp = in.readGE()
    val _lag  = in.readGE()
    new MouseY(_lo, _hi, _warp, _lag)
  }
}

/** A UGen that maps the vertical screen location of the mouse to a given linear or
  * exponential range. The `lo` value corresponds to the ''bottom'' of the screen,
  * and the `hi` value corresponds to the ''top'' of the screen (not vice-versa).
  * 
  * This UGen is useful for testing purposes. Mouse interaction with the regular
  * desktop and windowing system is in no way altered by running this UGen.
  * 
  * @param lo               value when the mouse is on the bottom side of the
  *                         screen. '''Note''': this value is never reached, because
  *                         the maximum mouse coordinate is one pixel less than the
  *                         screen height. For example, if the screen height is 900,
  *                         `lo` is 0.0 and `hi` is 1.0, the minimum value output is
  *                         `1.0 - 1.0 * 899.0/900 = 0.001111` .
  * @param hi               value when the mouse is on the top side of the screen
  * @param warp             curve shape. Either zero (default) for a linear
  *                         mapping, or 1 for an exponential mapping. '''Note''':
  *                         When using exponential mapping, make sure the `lo` value
  *                         is greater than zero, otherwise NaN values will be
  *                         output.
  * @param lag              60 dB decay time of a lag-time smoothing. Use zero to
  *                         avoid any smoothing.
  * 
  * @see [[de.sciss.synth.ugen.MouseX$ MouseX]]
  * @see [[de.sciss.synth.ugen.MouseButton$ MouseButton]]
  * @see [[de.sciss.synth.ugen.KeyState$ KeyState]]
  */
final case class MouseY(lo: GE = 0.0f, hi: GE = 1.0f, warp: GE = 0.0f, lag: GE = 0.2f)
  extends UGenSource.SingleOut with ControlRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, warp.expand, lag.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, control, _args)
}

/** A UGen that outputs two different values depending on whether the mouse button
  * is pressed. This is useful for testing purposes. Mouse interaction with the
  * regular desktop and windowing system is in no way altered by running this UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // toggle sine frequency
  * play { SinOsc.ar(MouseButton.kr(400, 440, 0.1)) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.MouseX$ MouseX]]
  * @see [[de.sciss.synth.ugen.MouseY$ MouseY]]
  * @see [[de.sciss.synth.ugen.KeyState$ KeyState]]
  */
object MouseButton extends ProductReader[MouseButton] {
  def kr: MouseButton = kr()
  
  /** @param lo               value output while button is not pressed
    * @param hi               value output while button is pressed
    * @param lag              60 dB decay time of a lag-time smoothing. Use zero to
    *                         avoid any smoothing.
    */
  def kr(lo: GE = 0.0f, hi: GE = 1.0f, lag: GE = 0.2f): MouseButton = new MouseButton(lo, hi, lag)
  
  def read(in: RefMapIn, key: String, arity: Int): MouseButton = {
    require (arity == 3)
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    val _lag  = in.readGE()
    new MouseButton(_lo, _hi, _lag)
  }
}

/** A UGen that outputs two different values depending on whether the mouse button
  * is pressed. This is useful for testing purposes. Mouse interaction with the
  * regular desktop and windowing system is in no way altered by running this UGen.
  * 
  * @param lo               value output while button is not pressed
  * @param hi               value output while button is pressed
  * @param lag              60 dB decay time of a lag-time smoothing. Use zero to
  *                         avoid any smoothing.
  * 
  * @see [[de.sciss.synth.ugen.MouseX$ MouseX]]
  * @see [[de.sciss.synth.ugen.MouseY$ MouseY]]
  * @see [[de.sciss.synth.ugen.KeyState$ KeyState]]
  */
final case class MouseButton(lo: GE = 0.0f, hi: GE = 1.0f, lag: GE = 0.2f)
  extends UGenSource.SingleOut with ControlRated {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](lo.expand, hi.expand, lag.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, control, _args)
}