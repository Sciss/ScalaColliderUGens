// revision: 10
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen which outputs a value of 1 for a given duration when triggered.
  * 
  * When a trigger occurs at the input, a value of 1 is output for the specified
  * duration, otherwise zero is output. When a new trigger occurs while this ugens
  * outputs 1, the hold-time is reset to the duration.
  * 
  * ''Warning:'' The hold-time is subject to a bug that depends on the input
  * signal. For example with `Trig1.ar(Impulse.ar(0), 4 * SampleDur.ir)` one
  * actually gets a high signal for five sample frames instead of four.
  * 
  * ===Examples===
  * 
  * {{{
  * // hold dust spikes
  * play { Trig1.ar(Dust.ar(1), 0.2) * SinOsc.ar(800) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Trig$ Trig]]
  */
object Trig1 extends ProductReader[Trig1] {
  /** @param in               the trigger. This can be any signal. A trigger happens
    *                         when the signal changes from non-positive to positive.
    * @param dur              the duration for which the ugens holds the value of 1
    *                         when triggered
    */
  def kr(in: GE, dur: GE = 0.1f): Trig1 = new Trig1(control, in, dur)
  
  /** @param in               the trigger. This can be any signal. A trigger happens
    *                         when the signal changes from non-positive to positive.
    * @param dur              the duration for which the ugens holds the value of 1
    *                         when triggered
    */
  def ar(in: GE, dur: GE = 0.1f): Trig1 = new Trig1(audio, in, dur)
  
  def read(in: RefMapIn, arity: Int): Trig1 = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _dur  = in.readGE()
    new Trig1(_rate, _in, _dur)
  }
}

/** A UGen which outputs a value of 1 for a given duration when triggered.
  * 
  * When a trigger occurs at the input, a value of 1 is output for the specified
  * duration, otherwise zero is output. When a new trigger occurs while this ugens
  * outputs 1, the hold-time is reset to the duration.
  * 
  * ''Warning:'' The hold-time is subject to a bug that depends on the input
  * signal. For example with `Trig1.ar(Impulse.ar(0), 4 * SampleDur.ir)` one
  * actually gets a high signal for five sample frames instead of four.
  * 
  * @param in               the trigger. This can be any signal. A trigger happens
  *                         when the signal changes from non-positive to positive.
  * @param dur              the duration for which the ugens holds the value of 1
  *                         when triggered
  * 
  * @see [[de.sciss.synth.ugen.Trig$ Trig]]
  */
final case class Trig1(rate: Rate, in: GE, dur: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, dur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen which holds and outputs an input value for a given duration when
  * triggered.
  * 
  * When a trigger occurs at the input, the input value is sampled and output for
  * the specified duration, otherwise zero is output. When a new trigger occurs
  * while this ugens outputs 1, the hold-time is reset to the duration.
  * 
  * ''Warning:'' The hold-time is subject to a bug that depends on the input
  * signal. For example with `Trig1.ar(Impulse.ar(0), 4 * SampleDur.ir)` one
  * actually gets a high signal for five sample frames instead of four.
  * 
  * ===Examples===
  * 
  * {{{
  * // hold dust spikes
  * play { Trig.ar(Dust.ar(1), 0.2) * SinOsc.ar(800) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Trig1$ Trig1]]
  */
object Trig extends ProductReader[Trig] {
  /** @param in               the trigger. This can be any signal. A trigger happens
    *                         when the signal changes from non-positive to positive.
    * @param dur              the duration for which the ugens holds the value of the
    *                         input signal when triggered
    */
  def kr(in: GE, dur: GE = 0.1f): Trig = new Trig(control, in, dur)
  
  /** @param in               the trigger. This can be any signal. A trigger happens
    *                         when the signal changes from non-positive to positive.
    * @param dur              the duration for which the ugens holds the value of the
    *                         input signal when triggered
    */
  def ar(in: GE, dur: GE = 0.1f): Trig = new Trig(audio, in, dur)
  
  def read(in: RefMapIn, arity: Int): Trig = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _dur  = in.readGE()
    new Trig(_rate, _in, _dur)
  }
}

/** A UGen which holds and outputs an input value for a given duration when
  * triggered.
  * 
  * When a trigger occurs at the input, the input value is sampled and output for
  * the specified duration, otherwise zero is output. When a new trigger occurs
  * while this ugens outputs 1, the hold-time is reset to the duration.
  * 
  * ''Warning:'' The hold-time is subject to a bug that depends on the input
  * signal. For example with `Trig1.ar(Impulse.ar(0), 4 * SampleDur.ir)` one
  * actually gets a high signal for five sample frames instead of four.
  * 
  * @param in               the trigger. This can be any signal. A trigger happens
  *                         when the signal changes from non-positive to positive.
  * @param dur              the duration for which the ugens holds the value of the
  *                         input signal when triggered
  * 
  * @see [[de.sciss.synth.ugen.Trig1$ Trig1]]
  */
final case class Trig(rate: Rate, in: GE, dur: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, dur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that sends a value from the server to all notified clients upon
  * receiving triggers. The message sent is
  * `osc.Message("/tr", <(Int) nodeId>, <(Int) trigId>, <(Float) value>)` .
  * 
  * For sending an array of values, or using an arbitrary reply command, see
  * `SendReply` .
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.SendReply$ SendReply]]
  */
object SendTrig extends ProductReader[SendTrig] {
  /** @param trig             the trigger signal causing the value to be read and
    *                         sent. A trigger occurs when passing from non-positive to
    *                         positive.
    * @param value            a changing signal or constant that will be polled at
    *                         the time of trigger, and its value passed with the
    *                         trigger message
    * @param id               an arbitrary integer that will be sent along with the
    *                         `"/tr"` message. This is useful to distinguish between
    *                         several SendTrig instances per SynthDef.
    */
  def kr(trig: GE, value: GE = 0.0f, id: GE = 0): SendTrig = new SendTrig(control, trig, value, id)
  
  /** @param trig             the trigger signal causing the value to be read and
    *                         sent. A trigger occurs when passing from non-positive to
    *                         positive.
    * @param value            a changing signal or constant that will be polled at
    *                         the time of trigger, and its value passed with the
    *                         trigger message
    * @param id               an arbitrary integer that will be sent along with the
    *                         `"/tr"` message. This is useful to distinguish between
    *                         several SendTrig instances per SynthDef.
    */
  def ar(trig: GE, value: GE = 0.0f, id: GE = 0): SendTrig = new SendTrig(audio, trig, value, id)
  
  def read(in: RefMapIn, arity: Int): SendTrig = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _trig   = in.readGE()
    val _value  = in.readGE()
    val _id     = in.readGE()
    new SendTrig(_rate, _trig, _value, _id)
  }
}

/** A UGen that sends a value from the server to all notified clients upon
  * receiving triggers. The message sent is
  * `osc.Message("/tr", <(Int) nodeId>, <(Int) trigId>, <(Float) value>)` .
  * 
  * For sending an array of values, or using an arbitrary reply command, see
  * `SendReply` .
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param trig             the trigger signal causing the value to be read and
  *                         sent. A trigger occurs when passing from non-positive to
  *                         positive.
  * @param value            a changing signal or constant that will be polled at
  *                         the time of trigger, and its value passed with the
  *                         trigger message
  * @param id               an arbitrary integer that will be sent along with the
  *                         `"/tr"` message. This is useful to distinguish between
  *                         several SendTrig instances per SynthDef.
  * 
  * @see [[de.sciss.synth.ugen.SendReply$ SendReply]]
  */
final case class SendTrig(rate: MaybeRate, trig: GE, value: GE = 0.0f, id: GE = 0)
  extends UGenSource.ZeroOut with HasSideEffect {

  protected def makeUGens: Unit = unwrap(this, Vector[UGenInLike](trig.expand, id.expand, value.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.ZeroOut(name, _rate, _args1)
  }
}

/** A UGen which sends an sequence of values from the server to all notified
  * clients upon receiving triggers. The message sent is
  * `osc.Message(<(String) msgName>, <(Int) nodeId>, <(Int) replyId>, <(Float) values>*)`
  * .
  * 
  * For sending a single value, `SendTrig` provides an alternative.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.SendTrig$ SendTrig]]
  */
object SendReply extends ProductReader[SendReply] {
  /** @param trig             a non-positive to positive transition triggers a message
    * @param values           a graph element comprising the signal channels to be
    *                         polled
    * @param msgName          a string specifying the OSC message's name. by
    *                         convention, this should start with a forward slash and
    *                         contain only 7-bit ascii characters.
    * @param id               an integer identifier which is contained in the reply
    *                         message. While you can distinguish different `SendReply`
    *                         instances from the same Synth by choosing different OSC
    *                         message names, depending on the application you may use
    *                         the same message name but different ids (similar to
    *                         `SendTrig` ).
    */
  def kr(trig: GE, values: GE, msgName: String = "/reply", id: GE = 0): SendReply = 
    new SendReply(control, trig, values, msgName, id)
  
  /** @param trig             a non-positive to positive transition triggers a message
    * @param values           a graph element comprising the signal channels to be
    *                         polled
    * @param msgName          a string specifying the OSC message's name. by
    *                         convention, this should start with a forward slash and
    *                         contain only 7-bit ascii characters.
    * @param id               an integer identifier which is contained in the reply
    *                         message. While you can distinguish different `SendReply`
    *                         instances from the same Synth by choosing different OSC
    *                         message names, depending on the application you may use
    *                         the same message name but different ids (similar to
    *                         `SendTrig` ).
    */
  def ar(trig: GE, values: GE, msgName: String = "/reply", id: GE = 0): SendReply = 
    new SendReply(audio, trig, values, msgName, id)
  
  def read(in: RefMapIn, arity: Int): SendReply = {
    require (arity == 5)
    val _rate     = in.readMaybeRate()
    val _trig     = in.readGE()
    val _values   = in.readGE()
    val _msgName  = in.readString()
    val _id       = in.readGE()
    new SendReply(_rate, _trig, _values, _msgName, _id)
  }
}

/** A UGen which sends an sequence of values from the server to all notified
  * clients upon receiving triggers. The message sent is
  * `osc.Message(<(String) msgName>, <(Int) nodeId>, <(Int) replyId>, <(Float) values>*)`
  * .
  * 
  * For sending a single value, `SendTrig` provides an alternative.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param trig             a non-positive to positive transition triggers a message
  * @param values           a graph element comprising the signal channels to be
  *                         polled
  * @param msgName          a string specifying the OSC message's name. by
  *                         convention, this should start with a forward slash and
  *                         contain only 7-bit ascii characters.
  * @param id               an integer identifier which is contained in the reply
  *                         message. While you can distinguish different `SendReply`
  *                         instances from the same Synth by choosing different OSC
  *                         message names, depending on the application you may use
  *                         the same message name but different ids (similar to
  *                         `SendTrig` ).
  * 
  * @see [[de.sciss.synth.ugen.SendTrig$ SendTrig]]
  */
final case class SendReply(rate: MaybeRate, trig: GE, values: GE, msgName: String = "/reply", id: GE = 0)
  extends UGenSource.ZeroOut with HasSideEffect {

  protected def makeUGens: Unit = 
    unwrap(this, Vector[UGenInLike](trig.expand, id.expand).++(stringArg(msgName).++(values.expand.outputs)))
  
  protected def makeUGen(_args: Vec[UGenIn]): Unit = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.ZeroOut(name, _rate, _args1)
  }
}

/** A UGen for printing the current output value of its input to the console.
  * 
  * @see [[de.sciss.synth.ugen.SendTrig$ SendTrig]]
  */
object Poll extends ProductReader[Poll] {
  /** @param trig             a non-positive to positive transition telling Poll to
    *                         return a value
    * @param in               the signal you want to poll
    * @param label            a string or symbol to be printed with the polled value
    * @param trigId           if greater then 0, a `"/tr"` OSC message is sent back
    *                         to the client (similar to `SendTrig` )
    */
  def kr(trig: GE, in: GE, label: String = "poll", trigId: GE = -1): Poll = 
    new Poll(control, trig, in, label, trigId)
  
  /** @param trig             a non-positive to positive transition telling Poll to
    *                         return a value
    * @param in               the signal you want to poll
    * @param label            a string or symbol to be printed with the polled value
    * @param trigId           if greater then 0, a `"/tr"` OSC message is sent back
    *                         to the client (similar to `SendTrig` )
    */
  def ar(trig: GE, in: GE, label: String = "poll", trigId: GE = -1): Poll = 
    new Poll(audio, trig, in, label, trigId)
  
  def read(in: RefMapIn, arity: Int): Poll = {
    require (arity == 5)
    val _rate   = in.readMaybeRate()
    val _trig   = in.readGE()
    val _in     = in.readGE()
    val _label  = in.readString()
    val _trigId = in.readGE()
    new Poll(_rate, _trig, _in, _label, _trigId)
  }
}

/** A UGen for printing the current output value of its input to the console.
  * 
  * @param trig             a non-positive to positive transition telling Poll to
  *                         return a value
  * @param in               the signal you want to poll
  * @param label            a string or symbol to be printed with the polled value
  * @param trigId           if greater then 0, a `"/tr"` OSC message is sent back
  *                         to the client (similar to `SendTrig` )
  * 
  * @see [[de.sciss.synth.ugen.SendTrig$ SendTrig]]
  */
final case class Poll(rate: MaybeRate, trig: GE, in: GE, label: String = "poll", trigId: GE = -1)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, in.expand, trigId.expand).++(stringArg(label)))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1, hasSideEffect = true)
  }
}

/** A UGen that toggles like a flip-flop between zero and one upon receiving a
  * trigger. The flip-flop initially outputs zero and changes to one when the first
  * trigger arrives.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-button toggle
  * play {
  *   // make sure lag is zero, otherwise the output
  *   // never falls back exactly to zero!
  *   val tr = MouseButton.kr(lag = 0)
  *   val ff = ToggleFF.kr(tr)
  *   SinOsc.ar(ff.mulAdd(400, 800)) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SetResetFF$ SetResetFF]]
  */
object ToggleFF extends ProductReader[ToggleFF] {
  /** @param trig             a signal to trigger the flip-flop. a trigger occurs
    *                         when the signal changes from non-positive to positive.
    */
  def kr(trig: GE): ToggleFF = new ToggleFF(control, trig)
  
  /** @param trig             a signal to trigger the flip-flop. a trigger occurs
    *                         when the signal changes from non-positive to positive.
    */
  def ar(trig: GE): ToggleFF = new ToggleFF(audio, trig)
  
  def read(in: RefMapIn, arity: Int): ToggleFF = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _trig = in.readGE()
    new ToggleFF(_rate, _trig)
  }
}

/** A UGen that toggles like a flip-flop between zero and one upon receiving a
  * trigger. The flip-flop initially outputs zero and changes to one when the first
  * trigger arrives.
  * 
  * @param trig             a signal to trigger the flip-flop. a trigger occurs
  *                         when the signal changes from non-positive to positive.
  * 
  * @see [[de.sciss.synth.ugen.SetResetFF$ SetResetFF]]
  */
final case class ToggleFF(rate: MaybeRate, trig: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A flip-flop UGen with two inputs, one (set) triggering an output of 1.0, the
  * other (reset) triggering an output of 0.0. Subsequent triggers happening within
  * the same input slot have no effect. If both inputs receive a trigger at the same
  * time, the ''reset'' input takes precedence.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-button toggle
  * play {
  *   // make sure lag is zero, otherwise the output
  *   // never falls back exactly to zero!
  *   val set   = MouseButton.kr(lag = 0)
  *   val reset = Impulse.kr(1)
  *   val ff    = SetResetFF.kr(set, reset)
  *   SinOsc.ar(ff.mulAdd(400, 800)) * 0.1
  * }
  * }}}
  * {{{
  * // limit trigger rate
  * play {
  *   // with a combination of TDelay and SetResetFF
  *   // we can build a filter that lets triggers
  *   // pass at a maximum rate.
  *   val dur   = 1.0  // minimum spacing between triggers
  *   val in    = Dust.ar(10)  // high frequency trigger
  *   val lim   = SetResetFF.ar(in, TDelay.ar(in, dur))
  *   val time  = Timer.ar(lim)
  *   time.poll(lim, "bang")
  *   ()
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.ToggleFF$ ToggleFF]]
  */
object SetResetFF extends ProductReader[SetResetFF] {
  /** @param trig             trigger that sets output to 1. A trigger happens when
    *                         the signal changes from non-positive to positive.
    * @param reset            trigger that sets output to 0. A trigger happens when
    *                         the signal changes from non-positive to positive.
    */
  def kr(trig: GE, reset: GE): SetResetFF = new SetResetFF(control, trig, reset)
  
  /** @param trig             trigger that sets output to 1. A trigger happens when
    *                         the signal changes from non-positive to positive.
    * @param reset            trigger that sets output to 0. A trigger happens when
    *                         the signal changes from non-positive to positive.
    */
  def ar(trig: GE, reset: GE): SetResetFF = new SetResetFF(audio, trig, reset)
  
  def read(in: RefMapIn, arity: Int): SetResetFF = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _trig   = in.readGE()
    val _reset  = in.readGE()
    new SetResetFF(_rate, _trig, _reset)
  }
}

/** A flip-flop UGen with two inputs, one (set) triggering an output of 1.0, the
  * other (reset) triggering an output of 0.0. Subsequent triggers happening within
  * the same input slot have no effect. If both inputs receive a trigger at the same
  * time, the ''reset'' input takes precedence.
  * 
  * @param trig             trigger that sets output to 1. A trigger happens when
  *                         the signal changes from non-positive to positive.
  * @param reset            trigger that sets output to 0. A trigger happens when
  *                         the signal changes from non-positive to positive.
  * 
  * @see [[de.sciss.synth.ugen.ToggleFF$ ToggleFF]]
  */
final case class SetResetFF(rate: MaybeRate, trig: GE, reset: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, reset.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A sample-and-hold UGen. When triggered, a new value is taken from the input and
  * hold until the next trigger occurs.
  * 
  * Before the first trigger is registered, this UGen outputs zero.
  * 
  * @see [[de.sciss.synth.ugen.Gate$ Gate]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object Latch extends ProductReader[Latch] {
  /** @param in               the input signal
    * @param trig             the trigger. The can be any signal. A trigger happens
    *                         when the signal changes from non-positive to positive.
    */
  def kr(in: GE, trig: GE = 1): Latch = new Latch(control, in, trig)
  
  /** @param in               the input signal
    * @param trig             the trigger. The can be any signal. A trigger happens
    *                         when the signal changes from non-positive to positive.
    */
  def ar(in: GE, trig: GE = 1): Latch = new Latch(audio, in, trig)
  
  def read(in: RefMapIn, arity: Int): Latch = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _trig = in.readGE()
    new Latch(_rate, _in, _trig)
  }
}

/** A sample-and-hold UGen. When triggered, a new value is taken from the input and
  * hold until the next trigger occurs.
  * 
  * Before the first trigger is registered, this UGen outputs zero.
  * 
  * @param in               the input signal
  * @param trig             the trigger. The can be any signal. A trigger happens
  *                         when the signal changes from non-positive to positive.
  * 
  * @see [[de.sciss.synth.ugen.Gate$ Gate]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class Latch(rate: Rate, in: GE, trig: GE = 1) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A gate or hold UGen. It allows the input signal value to pass when the `gate`
  * argument is positive, otherwise it holds last value.
  * 
  * Before the first high gate value is registered, this UGen outputs zero.
  * 
  * @see [[de.sciss.synth.ugen.Latch$ Latch]]
  */
object Gate extends ProductReader[Gate] {
  /** @param in               the input signal to gate
    * @param gate             the signal specifying whether to pass the input signal
    *                         (when greater than zero) or whether to close the gate
    *                         and hold the last value (when less than or equal to
    *                         zero)
    */
  def kr(in: GE, gate: GE): Gate = new Gate(control, in, gate)
  
  /** @param in               the input signal to gate
    * @param gate             the signal specifying whether to pass the input signal
    *                         (when greater than zero) or whether to close the gate
    *                         and hold the last value (when less than or equal to
    *                         zero)
    */
  def ar(in: GE, gate: GE): Gate = new Gate(audio, in, gate)
  
  def read(in: RefMapIn, arity: Int): Gate = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _gate = in.readGE()
    new Gate(_rate, _in, _gate)
  }
}

/** A gate or hold UGen. It allows the input signal value to pass when the `gate`
  * argument is positive, otherwise it holds last value.
  * 
  * Before the first high gate value is registered, this UGen outputs zero.
  * 
  * @param in               the input signal to gate
  * @param gate             the signal specifying whether to pass the input signal
  *                         (when greater than zero) or whether to close the gate
  *                         and hold the last value (when less than or equal to
  *                         zero)
  * 
  * @see [[de.sciss.synth.ugen.Latch$ Latch]]
  */
final case class Gate(rate: MaybeRate, in: GE, gate: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, gate.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A Schmidt trigger UGen. Initially it outputs zero. When the input signal rises
  * above `hi` , its output switches to 1.0, which is hold until the signal falls
  * below `lo` , switching the output again to 0.0. The produces a kind of
  * hysteresis behavior, preventing heavy oscillations in a noisy system which might
  * occur with a single-threshold trigger.
  */
object Schmidt extends ProductReader[Schmidt] {
  /** @param in               input signal to be analyzed
    * @param lo               low threshold
    * @param hi               high threshold
    */
  def kr(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Schmidt = new Schmidt(control, in, lo, hi)
  
  /** @param in               input signal to be analyzed
    * @param lo               low threshold
    * @param hi               high threshold
    */
  def ar(in: GE, lo: GE = 0.0f, hi: GE = 1.0f): Schmidt = new Schmidt(audio, in, lo, hi)
  
  def read(in: RefMapIn, arity: Int): Schmidt = {
    require (arity == 4)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    val _lo   = in.readGE()
    val _hi   = in.readGE()
    new Schmidt(_rate, _in, _lo, _hi)
  }
}

/** A Schmidt trigger UGen. Initially it outputs zero. When the input signal rises
  * above `hi` , its output switches to 1.0, which is hold until the signal falls
  * below `lo` , switching the output again to 0.0. The produces a kind of
  * hysteresis behavior, preventing heavy oscillations in a noisy system which might
  * occur with a single-threshold trigger.
  * 
  * @param in               input signal to be analyzed
  * @param lo               low threshold
  * @param hi               high threshold
  */
final case class Schmidt(rate: MaybeRate, in: GE, lo: GE = 0.0f, hi: GE = 1.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that decimates trigger by outputting one impulse each time a certain
  * number of triggers at its input have been received.
  * 
  * ===Examples===
  * 
  * {{{
  * // every two mouse-button clicks
  * play {
  *   val in  = MouseButton.kr(lag = 0)
  *   in.poll(in, "in")
  *   val out = PulseDivider.kr(in, 2, -1)
  *   out.poll(out, "out")
  *   ()
  * }
  * }}}
  * {{{
  * // rhythmic 1:4 pattern
  * play {
  *   val p = Impulse.ar(8)
  *   val d = PulseDivider.ar(p, 4)
  *   val a = SinOsc.ar(1200) * Decay2.ar(p, 0.005, 0.1) * 0.3
  *   val b = SinOsc.ar( 600) * Decay2.ar(d, 0.005, 0.5) * 0.3
  *   Seq(a, b)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PulseCount$ PulseCount]]
  * @see [[de.sciss.synth.ugen.Stepper$ Stepper]]
  */
object PulseDivider extends ProductReader[PulseDivider] {
  /** @param trig             a trigger occurs when the signal changes from
    *                         non-positive to positive.
    * @param div              decimation factor of the UGen. A value of 1 would cause
    *                         an output trigger for each input trigger, whereas a
    *                         value of 2 would cause an output trigger each time the
    *                         internal counter has seen two input triggers.
    * @param start            value of the internal counter. For example, if `div` is
    *                         `2` , then a `start` value of `0` (default) means that
    *                         the first output trigger happens after two input
    *                         triggers, a `start` value of `1` means that the first
    *                         output trigger happens after just one input trigger.
    *                         Negative values can increase the number of required
    *                         input triggers for the first output trigger. For
    *                         example, if `start` is `-1` , the first output trigger
    *                         happens after three input triggers.
    */
  def kr(trig: GE, div: GE = 2, start: GE = 0): PulseDivider = 
    new PulseDivider(control, trig, div, start)
  
  /** @param trig             a trigger occurs when the signal changes from
    *                         non-positive to positive.
    * @param div              decimation factor of the UGen. A value of 1 would cause
    *                         an output trigger for each input trigger, whereas a
    *                         value of 2 would cause an output trigger each time the
    *                         internal counter has seen two input triggers.
    * @param start            value of the internal counter. For example, if `div` is
    *                         `2` , then a `start` value of `0` (default) means that
    *                         the first output trigger happens after two input
    *                         triggers, a `start` value of `1` means that the first
    *                         output trigger happens after just one input trigger.
    *                         Negative values can increase the number of required
    *                         input triggers for the first output trigger. For
    *                         example, if `start` is `-1` , the first output trigger
    *                         happens after three input triggers.
    */
  def ar(trig: GE, div: GE = 2, start: GE = 0): PulseDivider = new PulseDivider(audio, trig, div, start)
  
  def read(in: RefMapIn, arity: Int): PulseDivider = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _trig   = in.readGE()
    val _div    = in.readGE()
    val _start  = in.readGE()
    new PulseDivider(_rate, _trig, _div, _start)
  }
}

/** A UGen that decimates trigger by outputting one impulse each time a certain
  * number of triggers at its input have been received.
  * 
  * @param trig             a trigger occurs when the signal changes from
  *                         non-positive to positive.
  * @param div              decimation factor of the UGen. A value of 1 would cause
  *                         an output trigger for each input trigger, whereas a
  *                         value of 2 would cause an output trigger each time the
  *                         internal counter has seen two input triggers.
  * @param start            value of the internal counter. For example, if `div` is
  *                         `2` , then a `start` value of `0` (default) means that
  *                         the first output trigger happens after two input
  *                         triggers, a `start` value of `1` means that the first
  *                         output trigger happens after just one input trigger.
  *                         Negative values can increase the number of required
  *                         input triggers for the first output trigger. For
  *                         example, if `start` is `-1` , the first output trigger
  *                         happens after three input triggers.
  * 
  * @see [[de.sciss.synth.ugen.PulseCount$ PulseCount]]
  * @see [[de.sciss.synth.ugen.Stepper$ Stepper]]
  */
final case class PulseDivider(rate: MaybeRate, trig: GE, div: GE = 2, start: GE = 0)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, div.expand, start.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that counts the number of triggers observed.
  * 
  * ===Examples===
  * 
  * {{{
  * // count mouse clicks, reset at 10
  * play {
  *   val tr    = MouseButton.kr(lag = 0)
  *   val reset = PulseDivider.kr(tr, 10)
  *   val c     = PulseCount.kr(tr, reset)
  *   c.poll(tr + Impulse.kr(0), "count")
  *   ()
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Stepper$ Stepper]]
  */
object PulseCount extends ProductReader[PulseCount] {
  /** @param trig             a trigger happens when the signal changes from
    *                         non-positive to positive
    * @param reset            when triggered, resets the counter to zero. When both
    *                         `trig` and `reset` are triggered at the same time, the
    *                         `reset` takes precedence (output will be zero).
    */
  def kr(trig: GE, reset: GE = 0): PulseCount = new PulseCount(control, trig, reset)
  
  /** @param trig             a trigger happens when the signal changes from
    *                         non-positive to positive
    * @param reset            when triggered, resets the counter to zero. When both
    *                         `trig` and `reset` are triggered at the same time, the
    *                         `reset` takes precedence (output will be zero).
    */
  def ar(trig: GE, reset: GE = 0): PulseCount = new PulseCount(audio, trig, reset)
  
  def read(in: RefMapIn, arity: Int): PulseCount = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _trig   = in.readGE()
    val _reset  = in.readGE()
    new PulseCount(_rate, _trig, _reset)
  }
}

/** A UGen that counts the number of triggers observed.
  * 
  * @param trig             a trigger happens when the signal changes from
  *                         non-positive to positive
  * @param reset            when triggered, resets the counter to zero. When both
  *                         `trig` and `reset` are triggered at the same time, the
  *                         `reset` takes precedence (output will be zero).
  * 
  * @see [[de.sciss.synth.ugen.Stepper$ Stepper]]
  */
final case class PulseCount(rate: MaybeRate, trig: GE, reset: GE = 0) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, reset.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A pulse counting UGen. Each trigger increments a counter which is output as a
  * signal. The counter wraps inside the interval from `lo` to `hi` (inclusive).
  * That if you use a `lo` other than zero, you might want to adjust `resetVal` as
  * well. `Stepper` always starts with the value in `resetVal` , no matter what `lo`
  * is or whether the `reset` trigger is high or not.
  * 
  * ===Examples===
  * 
  * {{{
  * // arpeggio
  * play {
  *   val tr    = Impulse.ar(10)
  *   val step  = Stepper.ar(tr, lo = 4, hi = 16)
  *   val freq  = step * 100
  *   SinOsc.ar(freq) * AmpComp.ar(freq) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PulseCount$ PulseCount]]
  */
object Stepper extends ProductReader[Stepper] {
  /** @param trig             The trigger signal which increments the counter. A
    *                         trigger happens when the signal changes from
    *                         non-positive to positive. Note that if the UGen is
    *                         created with the trigger initially high, the counter
    *                         will also be incremented immediately. Thus a
    *                         `Stepper.kr(Impulse.kr(1))` will begin by outputting `1`
    *                         . If you want to avoid this, you could their subtract
    *                         `Impulse.kr(0)` from the trigger input, or set
    *                         `resetVal` to `hi` . E.g.
    *                         `Stepper.kr(Impulse.kr(1), lo = 0, hi = 4, resetVal = 4)`
    *                         will produce the sequence 0, 1, 2, 4, 0, ...
    * @param reset            A trigger which resets the counter to `resetVal`
    *                         immediately.
    * @param lo               The minimum value output. For a decremental `step`
    *                         value, the counter jumps to `hi` if it were to fall
    *                         below `lo` .
    * @param hi               The maximum value output. For an incremental `step`
    *                         value, the counter jumps to `lo` if it were to rise
    *                         beyond `hi` . Note that values greater than `0x7FFFFFBF`
    *                         (the default) cause numeric overflow and the UGen to
    *                         malfunction.
    * @param step             The amount by which the counter increases or decreases
    *                         upon receiving triggers. Note that if you use a
    *                         decremental counter, still `lo` must be the minimum and
    *                         `hi` must be the maximum value output. If `lo` > `hi` ,
    *                         the UGen behaves wrongly. In the case of decremental
    *                         counter, set `resetVal` to `hi` . E.g. to count from 4
    *                         down to 0, use
    *                         `Stepper.kr(trig, lo = 0, hi = 4, step = -1, resetVal = 4)`
    *                         , or, if you want to ignore an initial high trigger, you
    *                         could do
    *                         `Stepper.kr(Impulse.kr(1), lo = 0, hi = 4, step = -1, resetVal = 0)`
    *                         -- so `resetVal` is `lo` but due to the initial trigger
    *                         from `Impulse` the `Stepper` will in fact start
    *                         outputting from `4` .
    */
  def kr(trig: GE, reset: GE = 0, lo: GE = 0, hi: GE = 2147483583, step: GE = 1, resetVal: GE = 0): Stepper = 
    new Stepper(control, trig, reset, lo, hi, step, resetVal)
  
  /** @param trig             The trigger signal which increments the counter. A
    *                         trigger happens when the signal changes from
    *                         non-positive to positive. Note that if the UGen is
    *                         created with the trigger initially high, the counter
    *                         will also be incremented immediately. Thus a
    *                         `Stepper.kr(Impulse.kr(1))` will begin by outputting `1`
    *                         . If you want to avoid this, you could their subtract
    *                         `Impulse.kr(0)` from the trigger input, or set
    *                         `resetVal` to `hi` . E.g.
    *                         `Stepper.kr(Impulse.kr(1), lo = 0, hi = 4, resetVal = 4)`
    *                         will produce the sequence 0, 1, 2, 4, 0, ...
    * @param reset            A trigger which resets the counter to `resetVal`
    *                         immediately.
    * @param lo               The minimum value output. For a decremental `step`
    *                         value, the counter jumps to `hi` if it were to fall
    *                         below `lo` .
    * @param hi               The maximum value output. For an incremental `step`
    *                         value, the counter jumps to `lo` if it were to rise
    *                         beyond `hi` . Note that values greater than `0x7FFFFFBF`
    *                         (the default) cause numeric overflow and the UGen to
    *                         malfunction.
    * @param step             The amount by which the counter increases or decreases
    *                         upon receiving triggers. Note that if you use a
    *                         decremental counter, still `lo` must be the minimum and
    *                         `hi` must be the maximum value output. If `lo` > `hi` ,
    *                         the UGen behaves wrongly. In the case of decremental
    *                         counter, set `resetVal` to `hi` . E.g. to count from 4
    *                         down to 0, use
    *                         `Stepper.kr(trig, lo = 0, hi = 4, step = -1, resetVal = 4)`
    *                         , or, if you want to ignore an initial high trigger, you
    *                         could do
    *                         `Stepper.kr(Impulse.kr(1), lo = 0, hi = 4, step = -1, resetVal = 0)`
    *                         -- so `resetVal` is `lo` but due to the initial trigger
    *                         from `Impulse` the `Stepper` will in fact start
    *                         outputting from `4` .
    */
  def ar(trig: GE, reset: GE = 0, lo: GE = 0, hi: GE = 2147483583, step: GE = 1, resetVal: GE = 0): Stepper = 
    new Stepper(audio, trig, reset, lo, hi, step, resetVal)
  
  def read(in: RefMapIn, arity: Int): Stepper = {
    require (arity == 7)
    val _rate     = in.readMaybeRate()
    val _trig     = in.readGE()
    val _reset    = in.readGE()
    val _lo       = in.readGE()
    val _hi       = in.readGE()
    val _step     = in.readGE()
    val _resetVal = in.readGE()
    new Stepper(_rate, _trig, _reset, _lo, _hi, _step, _resetVal)
  }
}

/** A pulse counting UGen. Each trigger increments a counter which is output as a
  * signal. The counter wraps inside the interval from `lo` to `hi` (inclusive).
  * That if you use a `lo` other than zero, you might want to adjust `resetVal` as
  * well. `Stepper` always starts with the value in `resetVal` , no matter what `lo`
  * is or whether the `reset` trigger is high or not.
  * 
  * @param trig             The trigger signal which increments the counter. A
  *                         trigger happens when the signal changes from
  *                         non-positive to positive. Note that if the UGen is
  *                         created with the trigger initially high, the counter
  *                         will also be incremented immediately. Thus a
  *                         `Stepper.kr(Impulse.kr(1))` will begin by outputting `1`
  *                         . If you want to avoid this, you could their subtract
  *                         `Impulse.kr(0)` from the trigger input, or set
  *                         `resetVal` to `hi` . E.g.
  *                         `Stepper.kr(Impulse.kr(1), lo = 0, hi = 4, resetVal = 4)`
  *                         will produce the sequence 0, 1, 2, 4, 0, ...
  * @param reset            A trigger which resets the counter to `resetVal`
  *                         immediately.
  * @param lo               The minimum value output. For a decremental `step`
  *                         value, the counter jumps to `hi` if it were to fall
  *                         below `lo` .
  * @param hi               The maximum value output. For an incremental `step`
  *                         value, the counter jumps to `lo` if it were to rise
  *                         beyond `hi` . Note that values greater than `0x7FFFFFBF`
  *                         (the default) cause numeric overflow and the UGen to
  *                         malfunction.
  * @param step             The amount by which the counter increases or decreases
  *                         upon receiving triggers. Note that if you use a
  *                         decremental counter, still `lo` must be the minimum and
  *                         `hi` must be the maximum value output. If `lo` > `hi` ,
  *                         the UGen behaves wrongly. In the case of decremental
  *                         counter, set `resetVal` to `hi` . E.g. to count from 4
  *                         down to 0, use
  *                         `Stepper.kr(trig, lo = 0, hi = 4, step = -1, resetVal = 4)`
  *                         , or, if you want to ignore an initial high trigger, you
  *                         could do
  *                         `Stepper.kr(Impulse.kr(1), lo = 0, hi = 4, step = -1, resetVal = 0)`
  *                         -- so `resetVal` is `lo` but due to the initial trigger
  *                         from `Impulse` the `Stepper` will in fact start
  *                         outputting from `4` .
  * 
  * @see [[de.sciss.synth.ugen.PulseCount$ PulseCount]]
  */
final case class Stepper(rate: MaybeRate, trig: GE, reset: GE = 0, lo: GE = 0, hi: GE = 2147483583, step: GE = 1, resetVal: GE = 0)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, reset.expand, lo.expand, hi.expand, step.expand, resetVal.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A delay UGen for trigger signals. Other than a normal buffer delay, any new
  * trigger arriving in the time between the previous trigger and the passing of the
  * delay time is ignored.
  */
object TDelay extends ProductReader[TDelay] {
  /** @param trig             The input trigger. A trigger is recognized when the
    *                         signal passes from non-positive to positive. Note that,
    *                         no matter what the amplitude of the input trigger is,
    *                         the UGen will output a delayed trigger of amplitude 1.0.
    * @param dur              The delay time in seconds.
    */
  def kr(trig: GE, dur: GE = 0.1f): TDelay = new TDelay(control, trig, dur)
  
  /** @param trig             The input trigger. A trigger is recognized when the
    *                         signal passes from non-positive to positive. Note that,
    *                         no matter what the amplitude of the input trigger is,
    *                         the UGen will output a delayed trigger of amplitude 1.0.
    * @param dur              The delay time in seconds.
    */
  def ar(trig: GE, dur: GE = 0.1f): TDelay = new TDelay(audio, trig, dur)
  
  def read(in: RefMapIn, arity: Int): TDelay = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _trig = in.readGE()
    val _dur  = in.readGE()
    new TDelay(_rate, _trig, _dur)
  }
}

/** A delay UGen for trigger signals. Other than a normal buffer delay, any new
  * trigger arriving in the time between the previous trigger and the passing of the
  * delay time is ignored.
  * 
  * @param trig             The input trigger. A trigger is recognized when the
  *                         signal passes from non-positive to positive. Note that,
  *                         no matter what the amplitude of the input trigger is,
  *                         the UGen will output a delayed trigger of amplitude 1.0.
  * @param dur              The delay time in seconds.
  */
final case class TDelay(rate: MaybeRate, trig: GE, dur: GE = 0.1f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, dur.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A pitch estimation UGen based on counting the zero-crossings of the input
  * signal. This is a very crude pitch follower, but can be useful in some
  * situations.
  * 
  * ===Examples===
  * 
  * {{{
  * // reconstruct sine frequency
  * play {
  *   val f1 = SinOsc.kr(0.2).mulAdd(600, 700).roundTo(100)
  *   val a  = SinOsc.ar(f1) * 0.1
  *   val f2 = ZeroCrossing.ar(a)
  *   f2.poll(10, "estimation")
  *   val b  = SinOsc.ar(f2) * 0.1
  *   Seq(a, b)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Pitch$ Pitch]]
  */
object ZeroCrossing extends ProductReader[ZeroCrossing] {
  /** @param in               signal to analyze
    */
  def kr(in: GE): ZeroCrossing = new ZeroCrossing(control, in)
  
  /** @param in               signal to analyze
    */
  def ar(in: GE): ZeroCrossing = new ZeroCrossing(audio, in)
  
  def read(in: RefMapIn, arity: Int): ZeroCrossing = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _in   = in.readGE()
    new ZeroCrossing(_rate, _in)
  }
}

/** A pitch estimation UGen based on counting the zero-crossings of the input
  * signal. This is a very crude pitch follower, but can be useful in some
  * situations.
  * 
  * @param in               signal to analyze
  * 
  * @see [[de.sciss.synth.ugen.Pitch$ Pitch]]
  */
final case class ZeroCrossing(rate: MaybeRate, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that returns time since last triggered. The time returned is in seconds
  * and is measured from the last received trigger. Note that currently it seems the
  * initial memory is at -1 sample, so for `Impulse.ar(1)` the result (at 44.1 kHz)
  * is 2.26757e-05, followed strangely by 1.00002, and then (as expected) 1.0.
  * 
  * @see [[de.sciss.synth.ugen.Sweep$ Sweep]]
  */
object Timer extends ProductReader[Timer] {
  /** @param trig             the trigger to update the output signal. A trigger
    *                         occurs when trig signal crosses from non-positive to
    *                         positive.
    */
  def kr(trig: GE): Timer = new Timer(control, trig)
  
  /** @param trig             the trigger to update the output signal. A trigger
    *                         occurs when trig signal crosses from non-positive to
    *                         positive.
    */
  def ar(trig: GE): Timer = new Timer(audio, trig)
  
  def read(in: RefMapIn, arity: Int): Timer = {
    require (arity == 2)
    val _rate = in.readMaybeRate()
    val _trig = in.readGE()
    new Timer(_rate, _trig)
  }
}

/** A UGen that returns time since last triggered. The time returned is in seconds
  * and is measured from the last received trigger. Note that currently it seems the
  * initial memory is at -1 sample, so for `Impulse.ar(1)` the result (at 44.1 kHz)
  * is 2.26757e-05, followed strangely by 1.00002, and then (as expected) 1.0.
  * 
  * @param trig             the trigger to update the output signal. A trigger
  *                         occurs when trig signal crosses from non-positive to
  *                         positive.
  * 
  * @see [[de.sciss.synth.ugen.Sweep$ Sweep]]
  */
final case class Timer(rate: MaybeRate, trig: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen which starts a linear raise from zero each time it is triggered.
  * 
  * When speed is one, one gets a continually-updating measurement of the time (in
  * seconds) since the last trigger.
  * 
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  * @see [[de.sciss.synth.ugen.Phasor$ Phasor]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  */
object Sweep extends ProductReader[Sweep] {
  /** @param trig             the trigger that restarts the ramp, when passing from
    *                         non-positive to positive
    * @param speed            the amount of increment of the output signal per
    *                         second. In SCLang this argument is named `rate` , while
    *                         ScalaCollider uses `speed` to avoid conflict with the
    *                         UGen's calculation rate.
    */
  def kr(trig: GE, speed: GE): Sweep = new Sweep(control, trig, speed)
  
  /** @param trig             the trigger that restarts the ramp, when passing from
    *                         non-positive to positive
    * @param speed            the amount of increment of the output signal per
    *                         second. In SCLang this argument is named `rate` , while
    *                         ScalaCollider uses `speed` to avoid conflict with the
    *                         UGen's calculation rate.
    */
  def ar(trig: GE, speed: GE): Sweep = new Sweep(audio, trig, speed)
  
  def read(in: RefMapIn, arity: Int): Sweep = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _trig   = in.readGE()
    val _speed  = in.readGE()
    new Sweep(_rate, _trig, _speed)
  }
}

/** A UGen which starts a linear raise from zero each time it is triggered.
  * 
  * When speed is one, one gets a continually-updating measurement of the time (in
  * seconds) since the last trigger.
  * 
  * @param trig             the trigger that restarts the ramp, when passing from
  *                         non-positive to positive
  * @param speed            the amount of increment of the output signal per
  *                         second. In SCLang this argument is named `rate` , while
  *                         ScalaCollider uses `speed` to avoid conflict with the
  *                         UGen's calculation rate.
  * 
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  * @see [[de.sciss.synth.ugen.Phasor$ Phasor]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  */
final case class Sweep(rate: Rate, trig: GE, speed: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, speed.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A linear repeating ramp UGen between start and end values. Using a trigger
  * input, it can be reset to a specific position. Upon reaching the end of its
  * ramp, `Phasor` will wrap back to its start value. '''Note''': Since `end` is
  * defined as the wrap point, its value is never actually output.
  * 
  * ===Examples===
  * 
  * {{{
  * // glissandi
  * play {
  *   // mouse-x controls phasor speed
  *   val freq  = MouseX.kr(0.2, 2, 1)
  *   // mouse button can be used to jump back
  *   val reset = MouseButton.kr(lag = 0)
  *   val p     = Phasor.ar(reset, freq / SampleRate.ir)
  *   SinOsc.ar(p.linLin(0, 1, 600, 1000)) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  * @see [[de.sciss.synth.ugen.Stepper$ Stepper]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  */
object Phasor extends ProductReader[Phasor] {
  def kr: Phasor = kr()
  
  /** @param trig             trigger signal that causes the phasor to jump to the
    *                         `resetVal` position
    * @param speed            amount of increment ''per sample frame''. I.e at a
    *                         speed of 1, each sample output by the UGen will be 1
    *                         greater than the preceding sample. To achieve a specific
    *                         frequency `f` in Hertz, use a speed value of
    *                         `f / SampleRate.ir` .
    * @param lo               start value of the ramp
    * @param hi               end value of the ramp (exclusive)
    * @param resetVal         value to jump to upon receiving a trigger in the `trig`
    *                         input
    */
  def kr(trig: GE = 0, speed: GE = 1.0f, lo: GE = 0.0f, hi: GE = 1.0f, resetVal: GE = 0.0f): Phasor = 
    new Phasor(control, trig, speed, lo, hi, resetVal)
  
  def ar: Phasor = ar()
  
  /** @param trig             trigger signal that causes the phasor to jump to the
    *                         `resetVal` position
    * @param speed            amount of increment ''per sample frame''. I.e at a
    *                         speed of 1, each sample output by the UGen will be 1
    *                         greater than the preceding sample. To achieve a specific
    *                         frequency `f` in Hertz, use a speed value of
    *                         `f / SampleRate.ir` .
    * @param lo               start value of the ramp
    * @param hi               end value of the ramp (exclusive)
    * @param resetVal         value to jump to upon receiving a trigger in the `trig`
    *                         input
    */
  def ar(trig: GE = 0, speed: GE = 1.0f, lo: GE = 0.0f, hi: GE = 1.0f, resetVal: GE = 0.0f): Phasor = 
    new Phasor(audio, trig, speed, lo, hi, resetVal)
  
  def read(in: RefMapIn, arity: Int): Phasor = {
    require (arity == 6)
    val _rate     = in.readRate()
    val _trig     = in.readGE()
    val _speed    = in.readGE()
    val _lo       = in.readGE()
    val _hi       = in.readGE()
    val _resetVal = in.readGE()
    new Phasor(_rate, _trig, _speed, _lo, _hi, _resetVal)
  }
}

/** A linear repeating ramp UGen between start and end values. Using a trigger
  * input, it can be reset to a specific position. Upon reaching the end of its
  * ramp, `Phasor` will wrap back to its start value. '''Note''': Since `end` is
  * defined as the wrap point, its value is never actually output.
  * 
  * @param trig             trigger signal that causes the phasor to jump to the
  *                         `resetVal` position
  * @param speed            amount of increment ''per sample frame''. I.e at a
  *                         speed of 1, each sample output by the UGen will be 1
  *                         greater than the preceding sample. To achieve a specific
  *                         frequency `f` in Hertz, use a speed value of
  *                         `f / SampleRate.ir` .
  * @param lo               start value of the ramp
  * @param hi               end value of the ramp (exclusive)
  * @param resetVal         value to jump to upon receiving a trigger in the `trig`
  *                         input
  * 
  * @see [[de.sciss.synth.ugen.Ramp$ Ramp]]
  * @see [[de.sciss.synth.ugen.Stepper$ Stepper]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  */
final case class Phasor(rate: Rate, trig: GE = 0, speed: GE = 1.0f, lo: GE = 0.0f, hi: GE = 1.0f, resetVal: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, speed.expand, lo.expand, hi.expand, resetVal.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen to measure a signal's peak amplitude. Technically, this UGen works like
  * `RunningMax` after the absolute value of the input signal is taken.
  * 
  * The UGen keeps an internal state that reflects the maximum absolute input value
  * observed. When a trigger occurs at the reset input, it first copies the current
  * maximum value to its output and then (quasi-simultaneously) resets its internal
  * state to the current absolute input value. This way, the peak value seen from
  * the outside at trigger time is the correct peak value up to that moment. See the
  * 'illustrate timing' example to understand this timing.
  * 
  * ===Examples===
  * 
  * {{{
  * // illustrate timing
  * play {
  *   val i  = Impulse.ar(0)
  *   // first impulse after 100ms
  *   val t1 = DelayN.ar(i * 1.0 , 0.100, 0.100)
  *   // one sample later
  *   val t2 = Delay1.ar(t1) * 0.5
  *   // another sample later
  *   val t3 = Delay1.ar(t2)
  *   val p  = Peak.ar(t1 + t2, t2)
  *   // at t1, peak has already seen t1
  *   p.poll(t1, "t1")
  *   // at t2, peak still reports 1.0, while internally resetting
  *   p.poll(t2, "t2")
  *   // at t3, we observe 0.5, therefore peak did reset at t2
  *   p.poll(t3, "t3")
  *   ()
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.RunningMin$ RunningMin]]
  * @see [[de.sciss.synth.ugen.RunningMax$ RunningMax]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  * @see [[de.sciss.synth.ugen.PeakFollower$ PeakFollower]]
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  */
object Peak extends ProductReader[Peak] {
  /** @param in               input signal to analyze
    * @param trig             resets the maximum observed value to the current
    *                         absolute value of the input signal
    */
  def kr(in: GE, trig: GE): Peak = new Peak(control, in, trig)
  
  /** @param in               input signal to analyze
    * @param trig             resets the maximum observed value to the current
    *                         absolute value of the input signal
    */
  def ar(in: GE, trig: GE): Peak = new Peak(audio, in, trig)
  
  def read(in: RefMapIn, arity: Int): Peak = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _trig = in.readGE()
    new Peak(_rate, _in, _trig)
  }
}

/** A UGen to measure a signal's peak amplitude. Technically, this UGen works like
  * `RunningMax` after the absolute value of the input signal is taken.
  * 
  * The UGen keeps an internal state that reflects the maximum absolute input value
  * observed. When a trigger occurs at the reset input, it first copies the current
  * maximum value to its output and then (quasi-simultaneously) resets its internal
  * state to the current absolute input value. This way, the peak value seen from
  * the outside at trigger time is the correct peak value up to that moment. See the
  * 'illustrate timing' example to understand this timing.
  * 
  * @param in               input signal to analyze
  * @param trig             resets the maximum observed value to the current
  *                         absolute value of the input signal
  * 
  * @see [[de.sciss.synth.ugen.RunningMin$ RunningMin]]
  * @see [[de.sciss.synth.ugen.RunningMax$ RunningMax]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  * @see [[de.sciss.synth.ugen.PeakFollower$ PeakFollower]]
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  */
final case class Peak(rate: Rate, in: GE, trig: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A UGen to measure a signal's minimum value between triggers.
  * 
  * The UGen keeps an internal state that reflects the minimum input value
  * observed. When a trigger occurs at the reset input, it first copies the current
  * minimum value to its output and then (quasi-simultaneously) resets its internal
  * state to the current input value.
  * 
  * ===Examples===
  * 
  * {{{
  * // illustrate timing
  * play {
  *   val n = BrownNoise.ar
  *   val t = Impulse.ar(4)
  *   val r = RunningMin.ar(n, t)
  *   // value at the moment the reset
  *   // is triggered
  *   n.poll(t, "cur")
  *   // this is the minimum of the
  *   // recent two input samples
  *   // (the one during reset and
  *   // the current one), therefore
  *   // equal or slightly less than
  *   // the 'cur' value
  *   r.poll(Delay1.ar(t), "min")
  *   ()
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.RunningMax$ RunningMax]]
  * @see [[de.sciss.synth.ugen.Peak$ Peak]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  */
object RunningMin extends ProductReader[RunningMin] {
  /** @param in               input signal to analyze
    * @param trig             resets the minimum observed value to the current value
    *                         of the input signal
    */
  def kr(in: GE, trig: GE): RunningMin = new RunningMin(control, in, trig)
  
  /** @param in               input signal to analyze
    * @param trig             resets the minimum observed value to the current value
    *                         of the input signal
    */
  def ar(in: GE, trig: GE): RunningMin = new RunningMin(audio, in, trig)
  
  def read(in: RefMapIn, arity: Int): RunningMin = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _trig = in.readGE()
    new RunningMin(_rate, _in, _trig)
  }
}

/** A UGen to measure a signal's minimum value between triggers.
  * 
  * The UGen keeps an internal state that reflects the minimum input value
  * observed. When a trigger occurs at the reset input, it first copies the current
  * minimum value to its output and then (quasi-simultaneously) resets its internal
  * state to the current input value.
  * 
  * @param in               input signal to analyze
  * @param trig             resets the minimum observed value to the current value
  *                         of the input signal
  * 
  * @see [[de.sciss.synth.ugen.RunningMax$ RunningMax]]
  * @see [[de.sciss.synth.ugen.Peak$ Peak]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  */
final case class RunningMin(rate: Rate, in: GE, trig: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A UGen to measure a signal's maximum value between triggers.
  * 
  * The UGen keeps an internal state that reflects the maximum input value
  * observed. When a trigger occurs at the reset input, it first copies the current
  * maximum value to its output and then (quasi-simultaneously) resets its internal
  * state to the current input value.
  * 
  * ===Examples===
  * 
  * {{{
  * // illustrate timing
  * play {
  *   val n = BrownNoise.ar
  *   val t = Impulse.ar(4)
  *   val r = RunningMax.ar(n, t)
  *   // value at the moment the reset
  *   // is triggered
  *   n.poll(t, "cur")
  *   // this is the maximum of the
  *   // recent two input samples
  *   // (the one during reset and
  *   // the current one), therefore
  *   // equal or slightly greater than
  *   // the 'cur' value
  *   r.poll(Delay1.ar(t), "max")
  *   ()
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.RunningMin$ RunningMin]]
  * @see [[de.sciss.synth.ugen.Peak$ Peak]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  */
object RunningMax extends ProductReader[RunningMax] {
  /** @param in               input signal to analyze
    * @param trig             resets the maximum observed value to the current value
    *                         of the input signal
    */
  def kr(in: GE, trig: GE): RunningMax = new RunningMax(control, in, trig)
  
  /** @param in               input signal to analyze
    * @param trig             resets the maximum observed value to the current value
    *                         of the input signal
    */
  def ar(in: GE, trig: GE): RunningMax = new RunningMax(audio, in, trig)
  
  def read(in: RefMapIn, arity: Int): RunningMax = {
    require (arity == 3)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _trig = in.readGE()
    new RunningMax(_rate, _in, _trig)
  }
}

/** A UGen to measure a signal's maximum value between triggers.
  * 
  * The UGen keeps an internal state that reflects the maximum input value
  * observed. When a trigger occurs at the reset input, it first copies the current
  * maximum value to its output and then (quasi-simultaneously) resets its internal
  * state to the current input value.
  * 
  * @param in               input signal to analyze
  * @param trig             resets the maximum observed value to the current value
  *                         of the input signal
  * 
  * @see [[de.sciss.synth.ugen.RunningMin$ RunningMin]]
  * @see [[de.sciss.synth.ugen.Peak$ Peak]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  */
final case class RunningMax(rate: Rate, in: GE, trig: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A UGen that continually reports the peak amplitude of the signal received at
  * the input. If the absolute input level drops below the observed peak value, this
  * value decreases by the factor given as `decay` parameter (but no more than the
  * current absolute input level).
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-controlled decay
  * play {
  *   val in    = Impulse.ar(2)
  *   val decay = MouseX.kr(0.995, 1.0001, 1).min(1.0)
  *   decay.poll(HPZ1.kr(decay).abs, "decay")
  *   val p     = PeakFollower.ar(in, decay)
  *   val tr    = Impulse.ar(20)
  *   val pm    = RunningMax.ar(p, tr)
  *   pm.roundTo(0.001).poll(20, "peak")
  *   in
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Peak$ Peak]]
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  */
object PeakFollower extends ProductReader[PeakFollower] {
  /** @param in               input signal to trace
    * @param decay            feedback coefficient controlling the release rate. This
    *                         should be less than one, otherwise the UGen may blow up.
    */
  def kr(in: GE, decay: GE = 0.999f): PeakFollower = new PeakFollower(control, in, decay)
  
  /** @param in               input signal to trace
    * @param decay            feedback coefficient controlling the release rate. This
    *                         should be less than one, otherwise the UGen may blow up.
    */
  def ar(in: GE, decay: GE = 0.999f): PeakFollower = new PeakFollower(audio, in, decay)
  
  def read(in: RefMapIn, arity: Int): PeakFollower = {
    require (arity == 3)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _decay  = in.readGE()
    new PeakFollower(_rate, _in, _decay)
  }
}

/** A UGen that continually reports the peak amplitude of the signal received at
  * the input. If the absolute input level drops below the observed peak value, this
  * value decreases by the factor given as `decay` parameter (but no more than the
  * current absolute input level).
  * 
  * @param in               input signal to trace
  * @param decay            feedback coefficient controlling the release rate. This
  *                         should be less than one, otherwise the UGen may blow up.
  * 
  * @see [[de.sciss.synth.ugen.Peak$ Peak]]
  * @see [[de.sciss.synth.ugen.Amplitude$ Amplitude]]
  * @see [[de.sciss.synth.ugen.RunningSum$ RunningSum]]
  * @see [[de.sciss.synth.ugen.LagUD$ LagUD]]
  */
final case class PeakFollower(rate: MaybeRate, in: GE, decay: GE = 0.999f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, decay.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A UGen that switches between two input signal depending on which is changing
  * more. Change is based on the absolute of the differentiation of the respective
  * signals.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-x versus mouse-y
  * play {
  *   val x   = MouseX.kr(lag = 1)
  *   val y   = MouseY.kr(lag = 1)
  *   val c   = MostChange.kr(x, y)
  *   val isX = c sig_== x
  *   val isY = 1 - isX
  *   // if X change stronger, modulate pan position
  *   val p   = LFTri.ar(c * 10 * isX)
  *   // if Y change stronger, modulate sine frequency
  *   val f   = LFTri.ar(c * 10 * isY).linExp(-1, 1, 100, 4000)
  *   // report current state
  *   c.poll(5, "c")
  *   x.poll(isX, "now X")
  *   y.poll(isY, "now Y")
  *   Pan2.ar(SinOsc.ar(f) * 0.1, p)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LeastChange$ LeastChange]]
  */
object MostChange extends ProductReader[MostChange] {
  /** @param a                first input signal to select from
    * @param b                second input signal to select from
    */
  def kr(a: GE, b: GE): MostChange = new MostChange(control, a, b)
  
  /** @param a                first input signal to select from
    * @param b                second input signal to select from
    */
  def ar(a: GE, b: GE): MostChange = new MostChange(audio, a, b)
  
  def read(in: RefMapIn, arity: Int): MostChange = {
    require (arity == 3)
    val _rate = in.readRate()
    val _a    = in.readGE()
    val _b    = in.readGE()
    new MostChange(_rate, _a, _b)
  }
}

/** A UGen that switches between two input signal depending on which is changing
  * more. Change is based on the absolute of the differentiation of the respective
  * signals.
  * 
  * @param a                first input signal to select from
  * @param b                second input signal to select from
  * 
  * @see [[de.sciss.synth.ugen.LeastChange$ LeastChange]]
  */
final case class MostChange(rate: Rate, a: GE, b: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](a.expand, b.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen that switches between two input signal depending on which is changing
  * less. Change is based on the absolute of the differentiation of the respective
  * signals.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-x versus mouse-y
  * play {
  *   val x   = MouseX.kr(lag = 1)
  *   val y   = MouseY.kr(lag = 1)
  *   val c   = LeastChange.kr(x, y)
  *   val isX = c sig_== x
  *   val isY = 1 - isX
  *   // if X change weaker, modulate pan position
  *   val p   = LFTri.ar(c * 10 * isX)
  *   // if Y change weaker, modulate sine frequency
  *   val f   = LFTri.ar(c * 10 * isY).linExp(-1, 1, 100, 4000)
  *   // report current state
  *   c.poll(5, "c")
  *   x.poll(isX, "now X")
  *   y.poll(isY, "now Y")
  *   Pan2.ar(SinOsc.ar(f) * 0.1, p)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.MostChange$ MostChange]]
  */
object LeastChange extends ProductReader[LeastChange] {
  /** @param a                first input signal to select from
    * @param b                second input signal to select from
    */
  def kr(a: GE, b: GE): LeastChange = new LeastChange(control, a, b)
  
  /** @param a                first input signal to select from
    * @param b                second input signal to select from
    */
  def ar(a: GE, b: GE): LeastChange = new LeastChange(audio, a, b)
  
  def read(in: RefMapIn, arity: Int): LeastChange = {
    require (arity == 3)
    val _rate = in.readMaybeRate()
    val _a    = in.readGE()
    val _b    = in.readGE()
    new LeastChange(_rate, _a, _b)
  }
}

/** A UGen that switches between two input signal depending on which is changing
  * less. Change is based on the absolute of the differentiation of the respective
  * signals.
  * 
  * @param a                first input signal to select from
  * @param b                second input signal to select from
  * 
  * @see [[de.sciss.synth.ugen.MostChange$ MostChange]]
  */
final case class LeastChange(rate: MaybeRate, a: GE, b: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](a.expand, b.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}

/** A sample-and-hold UGen that outputs the last value before the input changed
  * more than a threshold. Change is based on the absolute of the differentiation of
  * input signal.
  * 
  * ===Examples===
  * 
  * {{{
  * // distortion
  * play {
  *   val in     = SinOsc.ar(262)
  *   val thresh = MouseX.kr(1.0e-3, 2.0, 1, lag = 1)
  *   thresh.poll(5, "thresh")
  *   LeakDC.ar(LastValue.ar(in, thresh)) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Slew$ Slew]]
  */
object LastValue extends ProductReader[LastValue] {
  /** @param in               input signal to analyze and filter
    * @param thresh           threshold below which the input sign
    */
  def kr(in: GE, thresh: GE = 0.01f): LastValue = new LastValue(control, in, thresh)
  
  /** @param in               input signal to analyze and filter
    * @param thresh           threshold below which the input sign
    */
  def ar(in: GE, thresh: GE = 0.01f): LastValue = new LastValue(audio, in, thresh)
  
  def read(in: RefMapIn, arity: Int): LastValue = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _thresh = in.readGE()
    new LastValue(_rate, _in, _thresh)
  }
}

/** A sample-and-hold UGen that outputs the last value before the input changed
  * more than a threshold. Change is based on the absolute of the differentiation of
  * input signal.
  * 
  * @param in               input signal to analyze and filter
  * @param thresh           threshold below which the input sign
  * 
  * @see [[de.sciss.synth.ugen.Slew$ Slew]]
  */
final case class LastValue(rate: Rate, in: GE, thresh: GE = 0.01f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, thresh.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen which monitors another UGen to see when it is finished. Some UGens, such
  * as `PlayBuf` , `RecordBuf` , `Line` , `XLine` , `EnvGen` , `Linen` , `BufRd` ,
  * `BufWr` , `DbufRd` , and the Buffer delay UGens set a 'done' flag when they are
  * finished playing. This UGen echoes that flag as an explicit output signal when
  * it is set to track a particular UGen. When the tracked UGen changes to done, the
  * output signal changes from zero to one.
  * 
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  */
object Done extends ProductReader[Done] {
  /** @param src              the UGen to track
    */
  def kr(src: GE with HasDoneFlag): Done = new Done(src)
  
  def read(in: RefMapIn, arity: Int): Done = {
    require (arity == 1)
    val _src = in.readGEDone()
    new Done(_src)
  }
}

/** A UGen which monitors another UGen to see when it is finished. Some UGens, such
  * as `PlayBuf` , `RecordBuf` , `Line` , `XLine` , `EnvGen` , `Linen` , `BufRd` ,
  * `BufWr` , `DbufRd` , and the Buffer delay UGens set a 'done' flag when they are
  * finished playing. This UGen echoes that flag as an explicit output signal when
  * it is set to track a particular UGen. When the tracked UGen changes to done, the
  * output signal changes from zero to one.
  * 
  * @param src              the UGen to track
  * 
  * @see [[de.sciss.synth.ugen.PlayBuf$ PlayBuf]]
  * @see [[de.sciss.synth.ugen.Line$ Line]]
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  */
final case class Done(src: GE with HasDoneFlag)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](src.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}

/** A UGen which pauses and resumes another node. Note that the UGen initially
  * assumes the node is running, that is, if `gate` is initially 1, this will
  * '''not''' resume a paused node. Instead, the gate must go to zero and back to
  * one to resume the node. Additionally, this UGen will only cause action if the
  * gate value changes, that is, if the node is paused or resumed otherwise, this
  * UGen will not interfere with that action, unless the gate value is adjusted.
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.PauseSelf$ PauseSelf]]
  */
object Pause extends ProductReader[Pause] {
  /** @param gate             when 0, node is paused, when 1, node is resumed
    * @param node             the id of the node to be paused or resumed
    */
  def kr(gate: GE, node: GE): Pause = new Pause(gate, node)
  
  def read(in: RefMapIn, arity: Int): Pause = {
    require (arity == 2)
    val _gate = in.readGE()
    val _node = in.readGE()
    new Pause(_gate, _node)
  }
}

/** A UGen which pauses and resumes another node. Note that the UGen initially
  * assumes the node is running, that is, if `gate` is initially 1, this will
  * '''not''' resume a paused node. Instead, the gate must go to zero and back to
  * one to resume the node. Additionally, this UGen will only cause action if the
  * gate value changes, that is, if the node is paused or resumed otherwise, this
  * UGen will not interfere with that action, unless the gate value is adjusted.
  * 
  * @param gate             when 0, node is paused, when 1, node is resumed
  * @param node             the id of the node to be paused or resumed
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.PauseSelf$ PauseSelf]]
  */
final case class Pause(gate: GE, node: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](gate.expand, node.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}

/** A UGen that, when triggered, frees enclosing synth. It frees the enclosing
  * synth when the input signal crosses from non-positive to positive.
  * 
  * ''Note'' that if the trigger is initially high the UGen will not react. For
  * example, `FreeSelf.kr("foo".kr)` will not work if the control is initially `1` .
  * A work-around is to wrap the input in this case in a `Trig` object:
  * `FreeSelf.kr(Trig.kr("foo".kr))` . This is most likely a bug.
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.PauseSelf$ PauseSelf]]
  */
object FreeSelf extends ProductReader[FreeSelf] {
  /** @param trig             the input signal which will trigger the action.
    */
  def kr(trig: GE): FreeSelf = new FreeSelf(trig)
  
  def read(in: RefMapIn, arity: Int): FreeSelf = {
    require (arity == 1)
    val _trig = in.readGE()
    new FreeSelf(_trig)
  }
}

/** A UGen that, when triggered, frees enclosing synth. It frees the enclosing
  * synth when the input signal crosses from non-positive to positive.
  * 
  * ''Note'' that if the trigger is initially high the UGen will not react. For
  * example, `FreeSelf.kr("foo".kr)` will not work if the control is initially `1` .
  * A work-around is to wrap the input in this case in a `Trig` object:
  * `FreeSelf.kr(Trig.kr("foo".kr))` . This is most likely a bug.
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @param trig             the input signal which will trigger the action.
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.PauseSelf$ PauseSelf]]
  */
final case class FreeSelf(trig: GE) extends UGenSource.SingleOut with ControlRated with HasSideEffect {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}

/** A UGen that, when triggered, pauses enclosing synth. It pauses the enclosing
  * synth when the input signal crosses from non-positive to positive.
  * 
  * ''Note'' that if the trigger is initially high the UGen will not react. For
  * example, `PauseSelf.kr("foo".kr)` will not work if the control is initially `1`
  * . A work-around is to wrap the input in this case in a `Trig` object:
  * `PauseSelf.kr(Trig.kr("foo".kr))` . This is most likely a bug.
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @see [[de.sciss.synth.ugen.Pause$ Pause]]
  * @see [[de.sciss.synth.ugen.FreeSelf$ FreeSelf]]
  */
object PauseSelf extends ProductReader[PauseSelf] {
  /** @param trig             the input signal which will trigger the action.
    */
  def kr(trig: GE): PauseSelf = new PauseSelf(trig)
  
  def read(in: RefMapIn, arity: Int): PauseSelf = {
    require (arity == 1)
    val _trig = in.readGE()
    new PauseSelf(_trig)
  }
}

/** A UGen that, when triggered, pauses enclosing synth. It pauses the enclosing
  * synth when the input signal crosses from non-positive to positive.
  * 
  * ''Note'' that if the trigger is initially high the UGen will not react. For
  * example, `PauseSelf.kr("foo".kr)` will not work if the control is initially `1`
  * . A work-around is to wrap the input in this case in a `Trig` object:
  * `PauseSelf.kr(Trig.kr("foo".kr))` . This is most likely a bug.
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @param trig             the input signal which will trigger the action.
  * 
  * @see [[de.sciss.synth.ugen.Pause$ Pause]]
  * @see [[de.sciss.synth.ugen.FreeSelf$ FreeSelf]]
  */
final case class PauseSelf(trig: GE) extends UGenSource.SingleOut with ControlRated with HasSideEffect {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}

/** A UGen that, when triggered, frees a given node.
  * 
  * This UGen outputs its trig input signal for convenience.
  * 
  * @see [[de.sciss.synth.ugen.Pause$ Pause]]
  * @see [[de.sciss.synth.ugen.FreeSelf$ FreeSelf]]
  */
object Free extends ProductReader[Free] {
  /** @param trig             the trigger to cause the action
    * @param node             the id of the target node to free upon receiving the
    *                         trigger
    */
  def kr(trig: GE, node: GE): Free = new Free(trig, node)
  
  def read(in: RefMapIn, arity: Int): Free = {
    require (arity == 2)
    val _trig = in.readGE()
    val _node = in.readGE()
    new Free(_trig, _node)
  }
}

/** A UGen that, when triggered, frees a given node.
  * 
  * This UGen outputs its trig input signal for convenience.
  * 
  * @param trig             the trigger to cause the action
  * @param node             the id of the target node to free upon receiving the
  *                         trigger
  * 
  * @see [[de.sciss.synth.ugen.Pause$ Pause]]
  * @see [[de.sciss.synth.ugen.FreeSelf$ FreeSelf]]
  */
final case class Free(trig: GE, node: GE) extends UGenSource.SingleOut with ControlRated with HasSideEffect {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](trig.expand, node.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}

/** A UGen that, when its input UGen is finished, frees enclosing synth. This is
  * essentially a shortcut for `FreeSelf.kr(Done.kr(src))` , so instead of providing
  * a trigger signal it reads directly the done flag of an appropriate ugen (such as
  * `Line` or `PlayBuf` ).
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.FreeSelf$ FreeSelf]]
  * @see [[de.sciss.synth.ugen.PauseSelfWhenDone$ PauseSelfWhenDone]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  */
object FreeSelfWhenDone extends ProductReader[FreeSelfWhenDone] {
  /** @param src              the input UGen which when finished will trigger the
    *                         action.
    */
  def kr(src: GE with HasDoneFlag): FreeSelfWhenDone = new FreeSelfWhenDone(src)
  
  def read(in: RefMapIn, arity: Int): FreeSelfWhenDone = {
    require (arity == 1)
    val _src = in.readGEDone()
    new FreeSelfWhenDone(_src)
  }
}

/** A UGen that, when its input UGen is finished, frees enclosing synth. This is
  * essentially a shortcut for `FreeSelf.kr(Done.kr(src))` , so instead of providing
  * a trigger signal it reads directly the done flag of an appropriate ugen (such as
  * `Line` or `PlayBuf` ).
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @param src              the input UGen which when finished will trigger the
  *                         action.
  * 
  * @see [[de.sciss.synth.ugen.Free$ Free]]
  * @see [[de.sciss.synth.ugen.FreeSelf$ FreeSelf]]
  * @see [[de.sciss.synth.ugen.PauseSelfWhenDone$ PauseSelfWhenDone]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  */
final case class FreeSelfWhenDone(src: GE with HasDoneFlag)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](src.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}

/** A UGen that, when its input UGen is finished, pauses enclosing synth. This is
  * essentially a shortcut for `PauseSelf.kr(Done.kr(src))` , so instead of
  * providing a trigger signal it reads directly the done flag of an appropriate
  * ugen (such as `Line` or `PlayBuf` ).
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @see [[de.sciss.synth.ugen.Pause$ Pause]]
  * @see [[de.sciss.synth.ugen.PauseSelf$ PauseSelf]]
  * @see [[de.sciss.synth.ugen.FreeSelfWhenDone$ FreeSelfWhenDone]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  */
object PauseSelfWhenDone extends ProductReader[PauseSelfWhenDone] {
  /** @param src              the input UGen which when finished will trigger the
    *                         action.
    */
  def kr(src: GE with HasDoneFlag): PauseSelfWhenDone = new PauseSelfWhenDone(src)
  
  def read(in: RefMapIn, arity: Int): PauseSelfWhenDone = {
    require (arity == 1)
    val _src = in.readGEDone()
    new PauseSelfWhenDone(_src)
  }
}

/** A UGen that, when its input UGen is finished, pauses enclosing synth. This is
  * essentially a shortcut for `PauseSelf.kr(Done.kr(src))` , so instead of
  * providing a trigger signal it reads directly the done flag of an appropriate
  * ugen (such as `Line` or `PlayBuf` ).
  * 
  * This UGen outputs its input signal for convenience.
  * 
  * @param src              the input UGen which when finished will trigger the
  *                         action.
  * 
  * @see [[de.sciss.synth.ugen.Pause$ Pause]]
  * @see [[de.sciss.synth.ugen.PauseSelf$ PauseSelf]]
  * @see [[de.sciss.synth.ugen.FreeSelfWhenDone$ FreeSelfWhenDone]]
  * @see [[de.sciss.synth.ugen.Done$ Done]]
  */
final case class PauseSelfWhenDone(src: GE with HasDoneFlag)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](src.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true)
}