// revision: 7
package de.sciss.synth
package ugen

import UGenSource._

/** A stereo panorama UGen based on equal-power amplitude control. When in center
  * position ( `pos = 0` ), the signal is attenuated by sqrt(0.5) or approx. -3 dB.
  * 
  * ===Examples===
  * 
  * {{{
  * // periodic left-right oscillation
  * play { Pan2.ar(PinkNoise.ar(0.4), SinOsc.kr(0.25), 0.3) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LinPan2$ LinPan2]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  * @see [[de.sciss.synth.ugen.Pan4$ Pan4]]
  * @see [[de.sciss.synth.ugen.Balance2$ Balance2]]
  * @see [[de.sciss.synth.ugen.XFade2$ XFade2]]
  */
object Pan2 extends ProductReader[Pan2] {
  /** @param in               (monophonic) input signal to be panned
    * @param pos              panorama position between -1 (hard left) via 0 (center)
    *                         to +1 (hard right)
    * @param level            additional gain control
    */
  def kr(in: GE, pos: GE = 0.0f, level: GE = 1.0f): Pan2 = new Pan2(control, in, pos, level)
  
  /** @param in               (monophonic) input signal to be panned
    * @param pos              panorama position between -1 (hard left) via 0 (center)
    *                         to +1 (hard right)
    * @param level            additional gain control
    */
  def ar(in: GE, pos: GE = 0.0f, level: GE = 1.0f): Pan2 = new Pan2(audio, in, pos, level)
  
  def read(in: RefMapIn, arity: Int): Pan2 = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _pos    = in.readGE()
    val _level  = in.readGE()
    new Pan2(_rate, _in, _pos, _level)
  }
}

/** A stereo panorama UGen based on equal-power amplitude control. When in center
  * position ( `pos = 0` ), the signal is attenuated by sqrt(0.5) or approx. -3 dB.
  * 
  * @param in               (monophonic) input signal to be panned
  * @param pos              panorama position between -1 (hard left) via 0 (center)
  *                         to +1 (hard right)
  * @param level            additional gain control
  * 
  * @see [[de.sciss.synth.ugen.LinPan2$ LinPan2]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  * @see [[de.sciss.synth.ugen.Pan4$ Pan4]]
  * @see [[de.sciss.synth.ugen.Balance2$ Balance2]]
  * @see [[de.sciss.synth.ugen.XFade2$ XFade2]]
  */
final case class Pan2(rate: Rate, in: GE, pos: GE = 0.0f, level: GE = 1.0f) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, pos.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args1)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** A four channel equal-power panorama UGen. The outputs are in order `leftFront`
  * , `rightFront` , `leftBack` , `rightBack` .
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse controlled pan position
  * play {
  *   val x = MouseX.kr(-1, 1)
  *   val y = MouseY.kr(-1, 1)
  *   val p = Pan4.ar(PinkNoise.ar, x, y)
  *   // make a stereo mix with different timbre front and back
  *   val f = Resonz.ar(Seq(p.leftFront, p.rightFront), 4000, 0.2) * 4
  *   val r = Resonz.ar(Seq(p.leftBack , p.rightBack ), 1500, 0.2) * 4
  *   f + r
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  */
object Pan4 extends ProductReader[Pan4] {
  /** @param in               (monophonic) input signal to be panned
    * @param xpos             horizontal panorama position from -1 (left) to +1
    *                         (right)
    * @param ypos             front-to-back panorama position from -1 (back) to +1
    *                         (front)
    * @param level            additional gain control
    */
  def kr(in: GE, xpos: GE = 0.0f, ypos: GE = 0.0f, level: GE = 1.0f): Pan4 = 
    new Pan4(control, in, xpos, ypos, level)
  
  /** @param in               (monophonic) input signal to be panned
    * @param xpos             horizontal panorama position from -1 (left) to +1
    *                         (right)
    * @param ypos             front-to-back panorama position from -1 (back) to +1
    *                         (front)
    * @param level            additional gain control
    */
  def ar(in: GE, xpos: GE = 0.0f, ypos: GE = 0.0f, level: GE = 1.0f): Pan4 = 
    new Pan4(audio, in, xpos, ypos, level)
  
  def read(in: RefMapIn, arity: Int): Pan4 = {
    require (arity == 5)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _xpos   = in.readGE()
    val _ypos   = in.readGE()
    val _level  = in.readGE()
    new Pan4(_rate, _in, _xpos, _ypos, _level)
  }
}

/** A four channel equal-power panorama UGen. The outputs are in order `leftFront`
  * , `rightFront` , `leftBack` , `rightBack` .
  * 
  * @param in               (monophonic) input signal to be panned
  * @param xpos             horizontal panorama position from -1 (left) to +1
  *                         (right)
  * @param ypos             front-to-back panorama position from -1 (back) to +1
  *                         (front)
  * @param level            additional gain control
  * 
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  */
final case class Pan4(rate: Rate, in: GE, xpos: GE = 0.0f, ypos: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, xpos.expand, ypos.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.MultiOut(name, rate, Vector.fill(4)(rate), _args1)
  }
  
  def leftFront: GE = ChannelProxy(this, 0)
  
  def rightFront: GE = ChannelProxy(this, 1)
  
  def leftBack: GE = ChannelProxy(this, 2)
  
  def rightBack: GE = ChannelProxy(this, 3)
}

/** A stereo panorama UGen based on linear amplitude control. When in center
  * position ( `pos = 0` ), the signal is attenuated by 0.5 or approx. -6 dB.
  * 
  * ===Examples===
  * 
  * {{{
  * // periodic left-right oscillation
  * play { LinPan2.ar(PinkNoise.ar(0.4), SinOsc.kr(0.25), 0.3) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  * @see [[de.sciss.synth.ugen.Balance2$ Balance2]]
  * @see [[de.sciss.synth.ugen.LinXFade2$ LinXFade2]]
  */
object LinPan2 extends ProductReader[LinPan2] {
  /** @param in               (monophonic) input signal to be panned
    * @param pos              panorama position between -1 (hard left) via 0 (center)
    *                         to +1 (hard right)
    * @param level            additional gain control
    */
  def kr(in: GE, pos: GE = 0.0f, level: GE = 1.0f): LinPan2 = new LinPan2(control, in, pos, level)
  
  /** @param in               (monophonic) input signal to be panned
    * @param pos              panorama position between -1 (hard left) via 0 (center)
    *                         to +1 (hard right)
    * @param level            additional gain control
    */
  def ar(in: GE, pos: GE = 0.0f, level: GE = 1.0f): LinPan2 = new LinPan2(audio, in, pos, level)
  
  def read(in: RefMapIn, arity: Int): LinPan2 = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _pos    = in.readGE()
    val _level  = in.readGE()
    new LinPan2(_rate, _in, _pos, _level)
  }
}

/** A stereo panorama UGen based on linear amplitude control. When in center
  * position ( `pos = 0` ), the signal is attenuated by 0.5 or approx. -6 dB.
  * 
  * @param in               (monophonic) input signal to be panned
  * @param pos              panorama position between -1 (hard left) via 0 (center)
  *                         to +1 (hard right)
  * @param level            additional gain control
  * 
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.PanAz$ PanAz]]
  * @see [[de.sciss.synth.ugen.Balance2$ Balance2]]
  * @see [[de.sciss.synth.ugen.LinXFade2$ LinXFade2]]
  */
final case class LinPan2(rate: Rate, in: GE, pos: GE = 0.0f, level: GE = 1.0f) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, pos.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args1)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** An equal power two channel balancing UGen. It takes a left and right input
  * signal and attenuates them according to the `pos` value, producing again a
  * stereophonic output.
  * 
  * @see [[de.sciss.synth.ugen.XFade2$ XFade2]]
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  */
object Balance2 extends ProductReader[Balance2] {
  /** @param inL              The left input signal
    * @param inR              The right input signal
    * @param pos              The balance position from `-1` (left only, right muted)
    *                         to `+1` (right only, left muted). The curve follows an
    *                         equal power law, such that
    *                         `left.squared + right.squared == 1` , e.g. at the middle
    *                         position `0` , both channels are multiplied with factor
    *                         `sqrt(0.5) = 0.707 = -3 dB` .
    */
  def kr(inL: GE, inR: GE, pos: GE = 0.0f, level: GE = 1.0f): Balance2 = 
    new Balance2(control, inL, inR, pos, level)
  
  /** @param inL              The left input signal
    * @param inR              The right input signal
    * @param pos              The balance position from `-1` (left only, right muted)
    *                         to `+1` (right only, left muted). The curve follows an
    *                         equal power law, such that
    *                         `left.squared + right.squared == 1` , e.g. at the middle
    *                         position `0` , both channels are multiplied with factor
    *                         `sqrt(0.5) = 0.707 = -3 dB` .
    */
  def ar(inL: GE, inR: GE, pos: GE = 0.0f, level: GE = 1.0f): Balance2 = 
    new Balance2(audio, inL, inR, pos, level)
  
  def read(in: RefMapIn, arity: Int): Balance2 = {
    require (arity == 5)
    val _rate   = in.readRate()
    val _inL    = in.readGE()
    val _inR    = in.readGE()
    val _pos    = in.readGE()
    val _level  = in.readGE()
    new Balance2(_rate, _inL, _inR, _pos, _level)
  }
}

/** An equal power two channel balancing UGen. It takes a left and right input
  * signal and attenuates them according to the `pos` value, producing again a
  * stereophonic output.
  * 
  * @param inL              The left input signal
  * @param inR              The right input signal
  * @param pos              The balance position from `-1` (left only, right muted)
  *                         to `+1` (right only, left muted). The curve follows an
  *                         equal power law, such that
  *                         `left.squared + right.squared == 1` , e.g. at the middle
  *                         position `0` , both channels are multiplied with factor
  *                         `sqrt(0.5) = 0.707 = -3 dB` .
  * 
  * @see [[de.sciss.synth.ugen.XFade2$ XFade2]]
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  */
final case class Balance2(rate: Rate, inL: GE, inR: GE, pos: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inL.expand, inR.expand, pos.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args2)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** A UGen that can be used for rotating an ambisonic B-format sound field around
  * an axis. It uses an equal-power rotation so it also works well on stereo sounds.
  * It takes two audio inputs ( `x` , `y` ) and an angle control ( `pos` ). It
  * outputs again two channels, using these formulas:
  * {{{
  * xr = cos(angle) * x + sin(angle) * y
  * yr = cos(angle) * y - sin(angle) * x
  * }}}
  * where `angle = pos * Pi` . This allows, for example, the use of `LFSaw` to
  * create a continuous rotation around a circle.
  * '''Note''': Be careful when accessing the output channels. `xr` and `yr` are
  * the X and Y output channels, whereas `x` and `y` refers to the X and Y input
  * channel.
  * 
  * ===Examples===
  * 
  * {{{
  * // 4-channel rotation of opposite sounds
  * play {
  *   val p = WhiteNoise.ar(0.05)                     // first source
  *   val q = Mix(LFSaw.ar(Seq(200, 200.37))) * 0.03  // second source
  *   // B-format encode 2 signals at opposite sides of the circle
  *   val enc = PanB2.ar(p, -0.5) + PanB2.ar(q, +0.5)
  *   val Seq(w, x, y) = (0 to 2).map(enc out _)
  *   val rot = Rotate2.ar(x, y, MouseX.kr(-1, +1))
  *   // B-format decode to quad (front-left, front-right, rear-left, rear-right)
  *   DecodeB2.ar(4, w, rot.xr, rot.yr)
  * }
  * }}}
  */
object Rotate2 extends ProductReader[Rotate2] {
  /** @param x                B-format X input channel
    * @param y                B-format Y input channel
    * @param pos              angle to rotate around the circle, normalized between
    *                         -1 and +1. -1 and +1 corresponds to -180 and +180
    *                         degrees (behind), -0.5 is 90 degrees left, 0 is frontal,
    *                         +0.5 is 90 degrees right.
    */
  def kr(x: GE, y: GE, pos: GE = 0.0f): Rotate2 = new Rotate2(control, x, y, pos)
  
  /** @param x                B-format X input channel
    * @param y                B-format Y input channel
    * @param pos              angle to rotate around the circle, normalized between
    *                         -1 and +1. -1 and +1 corresponds to -180 and +180
    *                         degrees (behind), -0.5 is 90 degrees left, 0 is frontal,
    *                         +0.5 is 90 degrees right.
    */
  def ar(x: GE, y: GE, pos: GE = 0.0f): Rotate2 = new Rotate2(audio, x, y, pos)
  
  def read(in: RefMapIn, arity: Int): Rotate2 = {
    require (arity == 4)
    val _rate = in.readRate()
    val _x    = in.readGE()
    val _y    = in.readGE()
    val _pos  = in.readGE()
    new Rotate2(_rate, _x, _y, _pos)
  }
}

/** A UGen that can be used for rotating an ambisonic B-format sound field around
  * an axis. It uses an equal-power rotation so it also works well on stereo sounds.
  * It takes two audio inputs ( `x` , `y` ) and an angle control ( `pos` ). It
  * outputs again two channels, using these formulas:
  * {{{
  * xr = cos(angle) * x + sin(angle) * y
  * yr = cos(angle) * y - sin(angle) * x
  * }}}
  * where `angle = pos * Pi` . This allows, for example, the use of `LFSaw` to
  * create a continuous rotation around a circle.
  * '''Note''': Be careful when accessing the output channels. `xr` and `yr` are
  * the X and Y output channels, whereas `x` and `y` refers to the X and Y input
  * channel.
  * 
  * @param x                B-format X input channel
  * @param y                B-format Y input channel
  * @param pos              angle to rotate around the circle, normalized between
  *                         -1 and +1. -1 and +1 corresponds to -180 and +180
  *                         degrees (behind), -0.5 is 90 degrees left, 0 is frontal,
  *                         +0.5 is 90 degrees right.
  */
final case class Rotate2(rate: Rate, x: GE, y: GE, pos: GE = 0.0f) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](x.expand, y.expand, pos.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args2)
  }
  
  def xr: GE = ChannelProxy(this, 0)
  
  def yr: GE = ChannelProxy(this, 1)
}

/** An equal power two channel cross fading UGen. In center position ( `pan = 0` ),
  * both input signals are attenuated by sqrt(0.5) or approx. -3 dB.
  * 
  * @see [[de.sciss.synth.ugen.LinXFade2$ LinXFade2]]
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
object XFade2 extends ProductReader[XFade2] {
  /** @param inA              The first input signal
    * @param inB              The second input signal
    * @param pan              the cross-fade position from `-1` (only input A
    *                         audible) to `+1` (only input B audible)
    * @param level            An overall amplitude multiplier that is applied to the
    *                         output signal
    */
  def kr(inA: GE, inB: GE = 0.0f, pan: GE = 0.0f, level: GE = 1.0f): XFade2 = 
    new XFade2(control, inA, inB, pan, level)
  
  /** @param inA              The first input signal
    * @param inB              The second input signal
    * @param pan              the cross-fade position from `-1` (only input A
    *                         audible) to `+1` (only input B audible)
    * @param level            An overall amplitude multiplier that is applied to the
    *                         output signal
    */
  def ar(inA: GE, inB: GE = 0.0f, pan: GE = 0.0f, level: GE = 1.0f): XFade2 = 
    new XFade2(audio, inA, inB, pan, level)
  
  def read(in: RefMapIn, arity: Int): XFade2 = {
    require (arity == 5)
    val _rate   = in.readRate()
    val _inA    = in.readGE()
    val _inB    = in.readGE()
    val _pan    = in.readGE()
    val _level  = in.readGE()
    new XFade2(_rate, _inA, _inB, _pan, _level)
  }
}

/** An equal power two channel cross fading UGen. In center position ( `pan = 0` ),
  * both input signals are attenuated by sqrt(0.5) or approx. -3 dB.
  * 
  * @param inA              The first input signal
  * @param inB              The second input signal
  * @param pan              the cross-fade position from `-1` (only input A
  *                         audible) to `+1` (only input B audible)
  * @param level            An overall amplitude multiplier that is applied to the
  *                         output signal
  * 
  * @see [[de.sciss.synth.ugen.LinXFade2$ LinXFade2]]
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
final case class XFade2(rate: Rate, inA: GE, inB: GE = 0.0f, pan: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inA.expand, inB.expand, pan.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.SingleOut(name, rate, _args2)
  }
}

/** An linear two channel cross fading UGen. In center position ( `pan = 0` ), both
  * input signals are attenuated by 0.5 or approx. -6 dB.
  * 
  * @see [[de.sciss.synth.ugen.LinXFade2$ LinXFade2]]
  * @see [[de.sciss.synth.ugen.LinPan2$ LinPan2]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
object LinXFade2 extends ProductReader[LinXFade2] {
  /** @param inA              The first input signal
    * @param inB              The second input signal
    * @param pan              the cross-fade position from `-1` (only input A
    *                         audible) to `+1` (only input B audible)
    * @param level            An overall amplitude multiplier that is applied to the
    *                         output signal
    */
  def kr(inA: GE, inB: GE = 0.0f, pan: GE = 0.0f, level: GE = 1.0f): LinXFade2 = 
    new LinXFade2(control, inA, inB, pan, level)
  
  /** @param inA              The first input signal
    * @param inB              The second input signal
    * @param pan              the cross-fade position from `-1` (only input A
    *                         audible) to `+1` (only input B audible)
    * @param level            An overall amplitude multiplier that is applied to the
    *                         output signal
    */
  def ar(inA: GE, inB: GE = 0.0f, pan: GE = 0.0f, level: GE = 1.0f): LinXFade2 = 
    new LinXFade2(audio, inA, inB, pan, level)
  
  def read(in: RefMapIn, arity: Int): LinXFade2 = {
    require (arity == 5)
    val _rate   = in.readRate()
    val _inA    = in.readGE()
    val _inB    = in.readGE()
    val _pan    = in.readGE()
    val _level  = in.readGE()
    new LinXFade2(_rate, _inA, _inB, _pan, _level)
  }
}

/** An linear two channel cross fading UGen. In center position ( `pan = 0` ), both
  * input signals are attenuated by 0.5 or approx. -6 dB.
  * 
  * @param inA              The first input signal
  * @param inB              The second input signal
  * @param pan              the cross-fade position from `-1` (only input A
  *                         audible) to `+1` (only input B audible)
  * @param level            An overall amplitude multiplier that is applied to the
  *                         output signal
  * 
  * @see [[de.sciss.synth.ugen.LinXFade2$ LinXFade2]]
  * @see [[de.sciss.synth.ugen.LinPan2$ LinPan2]]
  * @see [[de.sciss.synth.ugen.XOut$ XOut]]
  */
final case class LinXFade2(rate: Rate, inA: GE, inB: GE = 0.0f, pan: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inA.expand, inB.expand, pan.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.SingleOut(name, rate, _args2)
  }
}

/** An Ambisonics B-format encoding UGen. B-format is the name for first order
  * Ambisonics which has four channels W, X, Y, Z. By omitting the elevation
  * control, we get a two dimensional planar encoded signal consisting only of the X
  * and Y channels.
  * 
  * Note that unlike `PanB2`, azimuth is in radians.
  * 
  * @see [[de.sciss.synth.ugen.PanB2$ PanB2]]
  * @see [[de.sciss.synth.ugen.DecodeB2$ DecodeB2]]
  */
object PanB extends ProductReader[PanB] {
  /** @param in               (monophonic) input signal to be encoded
    * @param azimuth          position around the circle in radians. -Pi/+Pi is
    *                         behind, -Pi/2 is left, 0 is front, +Pi/2 is right.
    * @param elevation        elevation in radians, from -Pi/2 (bottom) to +Pi/2 (top)
    * @param level            additional gain control
    */
  def kr(in: GE, azimuth: GE = 0.0f, elevation: GE = 0.0f, level: GE = 1.0f): PanB = 
    new PanB(control, in, azimuth, elevation, level)
  
  /** @param in               (monophonic) input signal to be encoded
    * @param azimuth          position around the circle in radians. -Pi/+Pi is
    *                         behind, -Pi/2 is left, 0 is front, +Pi/2 is right.
    * @param elevation        elevation in radians, from -Pi/2 (bottom) to +Pi/2 (top)
    * @param level            additional gain control
    */
  def ar(in: GE, azimuth: GE = 0.0f, elevation: GE = 0.0f, level: GE = 1.0f): PanB = 
    new PanB(audio, in, azimuth, elevation, level)
  
  def read(in: RefMapIn, arity: Int): PanB = {
    require (arity == 5)
    val _rate       = in.readRate()
    val _in         = in.readGE()
    val _azimuth    = in.readGE()
    val _elevation  = in.readGE()
    val _level      = in.readGE()
    new PanB(_rate, _in, _azimuth, _elevation, _level)
  }
}

/** An Ambisonics B-format encoding UGen. B-format is the name for first order
  * Ambisonics which has four channels W, X, Y, Z. By omitting the elevation
  * control, we get a two dimensional planar encoded signal consisting only of the X
  * and Y channels.
  * 
  * Note that unlike `PanB2`, azimuth is in radians.
  * 
  * @param in               (monophonic) input signal to be encoded
  * @param azimuth          position around the circle in radians. -Pi/+Pi is
  *                         behind, -Pi/2 is left, 0 is front, +Pi/2 is right.
  * @param elevation        elevation in radians, from -Pi/2 (bottom) to +Pi/2 (top)
  * @param level            additional gain control
  * 
  * @see [[de.sciss.synth.ugen.PanB2$ PanB2]]
  * @see [[de.sciss.synth.ugen.DecodeB2$ DecodeB2]]
  */
final case class PanB(rate: Rate, in: GE, azimuth: GE = 0.0f, elevation: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, azimuth.expand, elevation.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.MultiOut(name, rate, Vector.fill(4)(rate), _args1)
  }
  
  def w: GE = ChannelProxy(this, 0)
  
  def x: GE = ChannelProxy(this, 1)
  
  def y: GE = ChannelProxy(this, 2)
  
  def z: GE = ChannelProxy(this, 3)
}

/** A two dimensional Ambisonics B-format encoding UGen. B-format is the name for
  * first order Ambisonics which normally has four channels W, X, Y, Z. By omitting
  * the elevation control, we get a two dimensional planar encoded signal consisting
  * only of the W, X and Y channels.
  * 
  * Note that unlike `PanB`, azimuth is normalized between -1 and +1.
  * 
  * ===Examples===
  * 
  * {{{
  * // 4-channel rotation of opposite sounds
  * play {
  *   val p = WhiteNoise.ar(0.05)                     // first source
  *   val q = Mix(LFSaw.ar(Seq(200, 200.37))) * 0.03  // second source
  *   // B-format encode 2 signals at opposite sides of the circle
  *   val enc = PanB2.ar(p, -0.5) + PanB2.ar(q, +0.5)
  *   val Seq(w, x, y) = (0 to 2).map(enc out _)
  *   val rot = Rotate2.ar(x, y, MouseX.kr(-1, +1))
  *   // B-format decode to quad (front-left, front-right, rear-left, rear-right)
  *   DecodeB2.ar(4, w, rot.xr, rot.yr)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PanB$ PanB]]
  * @see [[de.sciss.synth.ugen.BiPanB2$ BiPanB2]]
  * @see [[de.sciss.synth.ugen.DecodeB2$ DecodeB2]]
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  */
object PanB2 extends ProductReader[PanB2] {
  /** @param in               (monophonic) input signal to be encoded
    * @param azimuth          position around the circle from -1 to +1. -1 and +1
    *                         correspond to -180/+180 degrees (behind), -0.5 is 90
    *                         degrees to the left, 0 is frontal, +0.5 is 90 degrees to
    *                         the right.
    * @param level            additional gain control
    */
  def kr(in: GE, azimuth: GE = 0.0f, level: GE = 1.0f): PanB2 = new PanB2(control, in, azimuth, level)
  
  /** @param in               (monophonic) input signal to be encoded
    * @param azimuth          position around the circle from -1 to +1. -1 and +1
    *                         correspond to -180/+180 degrees (behind), -0.5 is 90
    *                         degrees to the left, 0 is frontal, +0.5 is 90 degrees to
    *                         the right.
    * @param level            additional gain control
    */
  def ar(in: GE, azimuth: GE = 0.0f, level: GE = 1.0f): PanB2 = new PanB2(audio, in, azimuth, level)
  
  def read(in: RefMapIn, arity: Int): PanB2 = {
    require (arity == 4)
    val _rate     = in.readRate()
    val _in       = in.readGE()
    val _azimuth  = in.readGE()
    val _level    = in.readGE()
    new PanB2(_rate, _in, _azimuth, _level)
  }
}

/** A two dimensional Ambisonics B-format encoding UGen. B-format is the name for
  * first order Ambisonics which normally has four channels W, X, Y, Z. By omitting
  * the elevation control, we get a two dimensional planar encoded signal consisting
  * only of the W, X and Y channels.
  * 
  * Note that unlike `PanB`, azimuth is normalized between -1 and +1.
  * 
  * @param in               (monophonic) input signal to be encoded
  * @param azimuth          position around the circle from -1 to +1. -1 and +1
  *                         correspond to -180/+180 degrees (behind), -0.5 is 90
  *                         degrees to the left, 0 is frontal, +0.5 is 90 degrees to
  *                         the right.
  * @param level            additional gain control
  * 
  * @see [[de.sciss.synth.ugen.PanB$ PanB]]
  * @see [[de.sciss.synth.ugen.BiPanB2$ BiPanB2]]
  * @see [[de.sciss.synth.ugen.DecodeB2$ DecodeB2]]
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  */
final case class PanB2(rate: Rate, in: GE, azimuth: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, azimuth.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.MultiOut(name, rate, Vector.fill(3)(rate), _args1)
  }
  
  def w: GE = ChannelProxy(this, 0)
  
  def x: GE = ChannelProxy(this, 1)
  
  def y: GE = ChannelProxy(this, 2)
}

/** A two dimensional Ambisonics B-format encoder UGen for a two-channel input
  * signal. ambisonic B-format. It places the two input channels at opposite poles
  * of the 2D (W, X, Y) Ambisonics field. It is equivalent to:
  * {{{
  * PanB2(_, inA, azimuth, level) + PanB2(_, inB, azimuth + 1, level)
  * }}}
  * 
  * 
  * ===Examples===
  * 
  * {{{
  * // 4-channel rotation of opposite sounds
  * play {
  *   val p = WhiteNoise.ar(0.05)                     // first source
  *   val q = Mix(LFSaw.ar(Seq(200, 200.37))) * 0.03  // second source
  *   // B-format encode 2 signals at opposite sides of the circle
  *   val enc = BiPanB2.ar(p, q, MouseX.kr(-1, +1))
  *   // B-format decode to quad (front-left, front-right, rear-left, rear-right)
  *   DecodeB2.ar(4, enc.w, enc.x, enc.y)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PanB2$ PanB2]]
  * @see [[de.sciss.synth.ugen.DecodeB2$ DecodeB2]]
  */
object BiPanB2 extends ProductReader[BiPanB2] {
  /** @param inA              the first (monophonic) input signal, which will appear
    *                         opposite to the first second signal
    * @param inB              the second (monophonic) input signal, which will appear
    *                         opposite to the first input signal
    */
  def kr(inA: GE, inB: GE, azimuth: GE = 0.0f, level: GE = 1.0f): BiPanB2 = 
    new BiPanB2(control, inA, inB, azimuth, level)
  
  /** @param inA              the first (monophonic) input signal, which will appear
    *                         opposite to the first second signal
    * @param inB              the second (monophonic) input signal, which will appear
    *                         opposite to the first input signal
    */
  def ar(inA: GE, inB: GE, azimuth: GE = 0.0f, level: GE = 1.0f): BiPanB2 = 
    new BiPanB2(audio, inA, inB, azimuth, level)
  
  def read(in: RefMapIn, arity: Int): BiPanB2 = {
    require (arity == 5)
    val _rate     = in.readRate()
    val _inA      = in.readGE()
    val _inB      = in.readGE()
    val _azimuth  = in.readGE()
    val _level    = in.readGE()
    new BiPanB2(_rate, _inA, _inB, _azimuth, _level)
  }
}

/** A two dimensional Ambisonics B-format encoder UGen for a two-channel input
  * signal. ambisonic B-format. It places the two input channels at opposite poles
  * of the 2D (W, X, Y) Ambisonics field. It is equivalent to:
  * {{{
  * PanB2(_, inA, azimuth, level) + PanB2(_, inB, azimuth + 1, level)
  * }}}
  * 
  * 
  * @param inA              the first (monophonic) input signal, which will appear
  *                         opposite to the first second signal
  * @param inB              the second (monophonic) input signal, which will appear
  *                         opposite to the first input signal
  * 
  * @see [[de.sciss.synth.ugen.PanB2$ PanB2]]
  * @see [[de.sciss.synth.ugen.DecodeB2$ DecodeB2]]
  */
final case class BiPanB2(rate: Rate, inA: GE, inB: GE, azimuth: GE = 0.0f, level: GE = 1.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inA.expand, inB.expand, azimuth.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.MultiOut(name, rate, Vector.fill(3)(rate), _args2)
  }
  
  def w: GE = ChannelProxy(this, 0)
  
  def x: GE = ChannelProxy(this, 1)
  
  def y: GE = ChannelProxy(this, 2)
}

/** An azimuth-based panorama UGen. It uses vector-based-amplitude panning where
  * the arbitrary number of speakers is supposed to be distributed in a circle with
  * even spacing between them. It uses an equal-power-curve to transition between
  * adjacent speakers. '''Note''' the different default value for the `orient`
  * argument!
  * 
  * Use case: To spread an multi-channel input signal across an output bus with a
  * different number of channels, such that the first input channel is played on the
  * first output channel (no spread to adjacent channels) and the last input channel
  * is played to the last output channel (no spread to adjacent channels), you would
  * create a dedicated `PanAz` per input channel where the pan position is
  * `inChanIdx * 2f / (inChannels - 1) * (outChannels - 1) / outChannels` .
  * 
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.SplayAz$ SplayAz]]
  */
object PanAz extends ProductReader[PanAz] {
  /** @param numChannels      the number of output channels
    * @param in               the input signal
    * @param pos              the pan position. Channels are evenly spaced over a
    *                         cyclic period of 2.0. the output channel position is
    *                         `pos / 2 * numChannels + orient` . Thus, assuming an
    *                         `orient` of `0.0` , and `numChannels` being for example
    *                         `3` , a `pos` of `0*2.0/3 == 0.0` corresponds to the
    *                         first output channel, a `pos` of `1*2.0/3` corresponds
    *                         to the second output channel, a `pos` of `2*2.0/3=4.0/3`
    *                         corresponds to the third and last output channel, and a
    *                         `pos` of `3*2.0/3=2.0` completes the circle and wraps
    *                         again to the first channel. Using a bipolar pan
    *                         position, such as a sawtooth that ranges from -1 to +1,
    *                         all channels will be cyclically panned through. Must be
    *                         control rate.
    * @param level            a control rate level input (linear multiplier).
    * @param width            the width of the panning envelope. The default of 2.0
    *                         pans between pairs of adjacent speakers. Width values
    *                         greater than two will spread the pan over greater
    *                         numbers of speakers. Width values less than one will
    *                         leave silent gaps between speakers.
    * @param orient           the offset in the output channels regarding a pan
    *                         position of zero. Note that ScalaCollider uses a default
    *                         of zero which means that a pan pos of zero outputs the
    *                         signal exactly on the first output channel. This is
    *                         different in sclang where the default is 0.5 which means
    *                         that a pan position of zero will output the signal
    *                         between the first and second speaker. Accordingly, an
    *                         `orient` of `1.0` would result in a channel offset of
    *                         one, where a pan position of zero would output the
    *                         signal exactly on the second output channel, and so
    *                         forth.
    */
  def kr(numChannels: Int, in: GE, pos: GE = 0.0f, level: GE = 1.0f, width: GE = 2.0f, orient: GE = 0.0f): PanAz = 
    new PanAz(control, numChannels, in, pos, level, width, orient)
  
  /** @param numChannels      the number of output channels
    * @param in               the input signal
    * @param pos              the pan position. Channels are evenly spaced over a
    *                         cyclic period of 2.0. the output channel position is
    *                         `pos / 2 * numChannels + orient` . Thus, assuming an
    *                         `orient` of `0.0` , and `numChannels` being for example
    *                         `3` , a `pos` of `0*2.0/3 == 0.0` corresponds to the
    *                         first output channel, a `pos` of `1*2.0/3` corresponds
    *                         to the second output channel, a `pos` of `2*2.0/3=4.0/3`
    *                         corresponds to the third and last output channel, and a
    *                         `pos` of `3*2.0/3=2.0` completes the circle and wraps
    *                         again to the first channel. Using a bipolar pan
    *                         position, such as a sawtooth that ranges from -1 to +1,
    *                         all channels will be cyclically panned through. Must be
    *                         control rate.
    * @param level            a control rate level input (linear multiplier).
    * @param width            the width of the panning envelope. The default of 2.0
    *                         pans between pairs of adjacent speakers. Width values
    *                         greater than two will spread the pan over greater
    *                         numbers of speakers. Width values less than one will
    *                         leave silent gaps between speakers.
    * @param orient           the offset in the output channels regarding a pan
    *                         position of zero. Note that ScalaCollider uses a default
    *                         of zero which means that a pan pos of zero outputs the
    *                         signal exactly on the first output channel. This is
    *                         different in sclang where the default is 0.5 which means
    *                         that a pan position of zero will output the signal
    *                         between the first and second speaker. Accordingly, an
    *                         `orient` of `1.0` would result in a channel offset of
    *                         one, where a pan position of zero would output the
    *                         signal exactly on the second output channel, and so
    *                         forth.
    */
  def ar(numChannels: Int, in: GE, pos: GE = 0.0f, level: GE = 1.0f, width: GE = 2.0f, orient: GE = 0.0f): PanAz = 
    new PanAz(audio, numChannels, in, pos, level, width, orient)
  
  def read(in: RefMapIn, arity: Int): PanAz = {
    require (arity == 7)
    val _rate         = in.readRate()
    val _numChannels  = in.readInt()
    val _in           = in.readGE()
    val _pos          = in.readGE()
    val _level        = in.readGE()
    val _width        = in.readGE()
    val _orient       = in.readGE()
    new PanAz(_rate, _numChannels, _in, _pos, _level, _width, _orient)
  }
}

/** An azimuth-based panorama UGen. It uses vector-based-amplitude panning where
  * the arbitrary number of speakers is supposed to be distributed in a circle with
  * even spacing between them. It uses an equal-power-curve to transition between
  * adjacent speakers. '''Note''' the different default value for the `orient`
  * argument!
  * 
  * Use case: To spread an multi-channel input signal across an output bus with a
  * different number of channels, such that the first input channel is played on the
  * first output channel (no spread to adjacent channels) and the last input channel
  * is played to the last output channel (no spread to adjacent channels), you would
  * create a dedicated `PanAz` per input channel where the pan position is
  * `inChanIdx * 2f / (inChannels - 1) * (outChannels - 1) / outChannels` .
  * 
  * @param numChannels      the number of output channels
  * @param in               the input signal
  * @param pos              the pan position. Channels are evenly spaced over a
  *                         cyclic period of 2.0. the output channel position is
  *                         `pos / 2 * numChannels + orient` . Thus, assuming an
  *                         `orient` of `0.0` , and `numChannels` being for example
  *                         `3` , a `pos` of `0*2.0/3 == 0.0` corresponds to the
  *                         first output channel, a `pos` of `1*2.0/3` corresponds
  *                         to the second output channel, a `pos` of `2*2.0/3=4.0/3`
  *                         corresponds to the third and last output channel, and a
  *                         `pos` of `3*2.0/3=2.0` completes the circle and wraps
  *                         again to the first channel. Using a bipolar pan
  *                         position, such as a sawtooth that ranges from -1 to +1,
  *                         all channels will be cyclically panned through. Must be
  *                         control rate.
  * @param level            a control rate level input (linear multiplier).
  * @param width            the width of the panning envelope. The default of 2.0
  *                         pans between pairs of adjacent speakers. Width values
  *                         greater than two will spread the pan over greater
  *                         numbers of speakers. Width values less than one will
  *                         leave silent gaps between speakers.
  * @param orient           the offset in the output channels regarding a pan
  *                         position of zero. Note that ScalaCollider uses a default
  *                         of zero which means that a pan pos of zero outputs the
  *                         signal exactly on the first output channel. This is
  *                         different in sclang where the default is 0.5 which means
  *                         that a pan position of zero will output the signal
  *                         between the first and second speaker. Accordingly, an
  *                         `orient` of `1.0` would result in a channel offset of
  *                         one, where a pan position of zero would output the
  *                         signal exactly on the second output channel, and so
  *                         forth.
  * 
  * @see [[de.sciss.synth.ugen.Pan2$ Pan2]]
  * @see [[de.sciss.synth.ugen.SplayAz$ SplayAz]]
  */
final case class PanAz(rate: Rate, numChannels: Int, in: GE, pos: GE = 0.0f, level: GE = 1.0f, width: GE = 2.0f, orient: GE = 0.0f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, pos.expand, level.expand, width.expand, orient.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = matchRate(_args1, 1, control)
    UGen.MultiOut(name, rate, Vector.fill(numChannels)(rate), _args2)
  }
}

/** A two dimensional Ambisonics B-format decoding UGen. It assumes a set of
  * speakers in a regular polygon. The output channels are in clockwise order. The
  * position of the first speaker is specified by the `orient` argument.
  * 
  * ===Examples===
  * 
  * {{{
  * // 4-channel rotation of opposite sounds
  * play {
  *   val p = WhiteNoise.ar(0.05)                     // first source
  *   val q = Mix(LFSaw.ar(Seq(200, 200.37))) * 0.03  // second source
  *   // B-format encode 2 signals at opposite sides of the circle
  *   val enc = PanB2.ar(p, -0.5) + PanB2.ar(q, +0.5)
  *   val Seq(w, x, y) = (0 to 2).map(enc out _)
  *   val rot = Rotate2.ar(x, y, MouseX.kr(-1, +1))
  *   // B-format decode to quad (front-left, front-right, rear-left, rear-right)
  *   DecodeB2.ar(4, w, rot.xr, rot.yr)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PanB$ PanB]]
  * @see [[de.sciss.synth.ugen.PanB2$ PanB2]]
  */
object DecodeB2 extends ProductReader[DecodeB2] {
  /** @param numChannels      the number of output channels to produce
    * @param w                W (first) channel of B-format input signal
    * @param x                X (second) channel of B-format input signal
    * @param y                Y (third) channel of B-format input signal
    * @param orient           orientation of the first channel. If zero, the first
    *                         channel corresponds to the front vertex of the polygon.
    *                         If the polygon does not have an edge at the front but a
    *                         vertex, then an `orient` of 0.5 indicates that the first
    *                         channel corresponds to the speaker left of the center.
    */
  def kr(numChannels: Int, w: GE, x: GE, y: GE, orient: GE = 0.5f): DecodeB2 = 
    new DecodeB2(control, numChannels, w, x, y, orient)
  
  /** @param numChannels      the number of output channels to produce
    * @param w                W (first) channel of B-format input signal
    * @param x                X (second) channel of B-format input signal
    * @param y                Y (third) channel of B-format input signal
    * @param orient           orientation of the first channel. If zero, the first
    *                         channel corresponds to the front vertex of the polygon.
    *                         If the polygon does not have an edge at the front but a
    *                         vertex, then an `orient` of 0.5 indicates that the first
    *                         channel corresponds to the speaker left of the center.
    */
  def ar(numChannels: Int, w: GE, x: GE, y: GE, orient: GE = 0.5f): DecodeB2 = 
    new DecodeB2(audio, numChannels, w, x, y, orient)
  
  def read(in: RefMapIn, arity: Int): DecodeB2 = {
    require (arity == 6)
    val _rate         = in.readRate()
    val _numChannels  = in.readInt()
    val _w            = in.readGE()
    val _x            = in.readGE()
    val _y            = in.readGE()
    val _orient       = in.readGE()
    new DecodeB2(_rate, _numChannels, _w, _x, _y, _orient)
  }
}

/** A two dimensional Ambisonics B-format decoding UGen. It assumes a set of
  * speakers in a regular polygon. The output channels are in clockwise order. The
  * position of the first speaker is specified by the `orient` argument.
  * 
  * @param numChannels      the number of output channels to produce
  * @param w                W (first) channel of B-format input signal
  * @param x                X (second) channel of B-format input signal
  * @param y                Y (third) channel of B-format input signal
  * @param orient           orientation of the first channel. If zero, the first
  *                         channel corresponds to the front vertex of the polygon.
  *                         If the polygon does not have an edge at the front but a
  *                         vertex, then an `orient` of 0.5 indicates that the first
  *                         channel corresponds to the speaker left of the center.
  * 
  * @see [[de.sciss.synth.ugen.PanB$ PanB]]
  * @see [[de.sciss.synth.ugen.PanB2$ PanB2]]
  */
final case class DecodeB2(rate: Rate, numChannels: Int, w: GE, x: GE, y: GE, orient: GE = 0.5f)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](w.expand, x.expand, y.expand, orient.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (rate.==(audio)) matchRate(_args1, 1, audio) else _args1
    val _args3 = if (rate.==(audio)) matchRate(_args2, 2, audio) else _args2
    UGen.MultiOut(name, rate, Vector.fill(numChannels)(rate), _args3)
  }
}