// revision: 2
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that detects a specific keyboard stroke. When the given key is not
  * pressed, the `lo` value is output, while the key is pressed the `hi` value is
  * output. If `lag` is greater than zero, a `Lag` -type operation is applied for a
  * smoother transition between `lo` and `hi` .
  * 
  * ===Examples===
  * 
  * {{{
  * // determine codes
  * play {
  *   val code = Phasor.kr(lo = 0, hi = 127)
  *   val k    = KeyState.kr(code, lag = 0)
  *   val x    = Gate.kr(code, k)
  *   val ch   = x sig_!= Delay1.kr(x)
  *   // when a code change is detected, print it
  *   x.poll(ch, "code")
  *   ()
  * }
  * }}}
  * {{{
  * // gate sound using the 'A' key
  * play {
  *   // on Linux, key-code 38 denotes the 'A' key
  *   SinOsc.ar(800) * KeyState.kr(38, 0, 0.1)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.MouseX$ MouseX]]
  * @see [[de.sciss.synth.ugen.MouseY$ MouseY]]
  * @see [[de.sciss.synth.ugen.MouseButton$ MouseButton]]
  */
object KeyState extends ProductReader[KeyState] {
  /** @param keyCode          hardware code for the key to monitor. This is likely
    *                         platform dependent. For example, on Linux, key-codes 24
    *                         to 29 correspond to 'Q', 'W', 'E', 'R', 'T', 'Y'.
    * @param lo               value output when the currently pressed does not match
    *                         the code
    * @param hi               value output when the currently pressed does match the
    *                         code
    * @param lag              60 dB lag time in seconds.
    */
  def kr(keyCode: GE, lo: GE = 0.0f, hi: GE = 1.0f, lag: GE = 0.2f): KeyState = 
    new KeyState(keyCode, lo, hi, lag)
  
  def read(in: RefMapIn, arity: Int): KeyState = {
    require (arity == 4)
    val _keyCode  = in.readGE()
    val _lo       = in.readGE()
    val _hi       = in.readGE()
    val _lag      = in.readGE()
    new KeyState(_keyCode, _lo, _hi, _lag)
  }
}

/** A UGen that detects a specific keyboard stroke. When the given key is not
  * pressed, the `lo` value is output, while the key is pressed the `hi` value is
  * output. If `lag` is greater than zero, a `Lag` -type operation is applied for a
  * smoother transition between `lo` and `hi` .
  * 
  * @param keyCode          hardware code for the key to monitor. This is likely
  *                         platform dependent. For example, on Linux, key-codes 24
  *                         to 29 correspond to 'Q', 'W', 'E', 'R', 'T', 'Y'.
  * @param lo               value output when the currently pressed does not match
  *                         the code
  * @param hi               value output when the currently pressed does match the
  *                         code
  * @param lag              60 dB lag time in seconds.
  * 
  * @see [[de.sciss.synth.ugen.MouseX$ MouseX]]
  * @see [[de.sciss.synth.ugen.MouseY$ MouseY]]
  * @see [[de.sciss.synth.ugen.MouseButton$ MouseButton]]
  */
final case class KeyState(keyCode: GE, lo: GE = 0.0f, hi: GE = 1.0f, lag: GE = 0.2f)
  extends UGenSource.SingleOut with ControlRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](keyCode.expand, lo.expand, hi.expand, lag.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, control, _args)
}