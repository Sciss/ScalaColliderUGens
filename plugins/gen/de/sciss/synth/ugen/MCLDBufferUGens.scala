// revision: 10
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen to store values in a buffer upon receiving a trigger. When a trigger
  * happens, the current input values are sampled and stored as the next consecutive
  * frame of the buffer.
  * 
  * Storage starts at the buffer beginning and increments the write position until
  * the buffer is full. While the buffer is not yet full, the UGen outputs `1` ,
  * then it outputs `0` . The buffer position can be reset using the `reset` input.
  * 
  * Note that the UGen zeroes the buffer upon first instantiation, to ensure that
  * out-of-date data is not confused with new data.
  * 
  * ===Examples===
  * 
  * {{{
  * // fill buffer and plot on client side
  * val b = Buffer(s)
  * b.alloc(100)
  * 
  * val x = play {
  *   val z = LFCub.kr(10) * EnvGen.kr(Env.linen(1, 2, 1), doneAction = freeSelf)
  *   Logger.kr(b.id, z, Impulse.kr(49), reset = 0)
  * }
  * 
  * // after synth has completed:
  * b.getData(num = 100).foreach(_.plot())
  * 
  * x.free(); b.free()
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.ListTrig$ ListTrig]]
  */
object Logger extends ProductReader[Logger] {
  /** @param buf              identifier of the buffer to write to. Its number of
    *                         channels should match those of `in` .
    * @param in               (multi-channel) signal to write to the buffer. Its
    *                         number of channels should match those of `buf` .
    * @param trig             a non-positive to positive transition causes the UGen
    *                         to append the current input values to the buffer
    * @param reset            a non-positive to positive transition causes the write
    *                         index into the buffer to be reset to zero. The contents
    *                         of the buffer will also be filled with zeroes. If the
    *                         buffer was full, the UGen output switches back to zero.
    */
  def kr(buf: GE, in: GE, trig: GE, reset: GE = 0): Logger = new Logger(control, buf, in, trig, reset)
  
  def read(in: RefMapIn, prefix: String, arity: Int): Logger = {
    require (arity == 5)
    val _rate   = in.readRate()
    val _buf    = in.readGE()
    val _in     = in.readGE()
    val _trig   = in.readGE()
    val _reset  = in.readGE()
    new Logger(_rate, _buf, _in, _trig, _reset)
  }
}

/** A UGen to store values in a buffer upon receiving a trigger. When a trigger
  * happens, the current input values are sampled and stored as the next consecutive
  * frame of the buffer.
  * 
  * Storage starts at the buffer beginning and increments the write position until
  * the buffer is full. While the buffer is not yet full, the UGen outputs `1` ,
  * then it outputs `0` . The buffer position can be reset using the `reset` input.
  * 
  * Note that the UGen zeroes the buffer upon first instantiation, to ensure that
  * out-of-date data is not confused with new data.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param buf              identifier of the buffer to write to. Its number of
  *                         channels should match those of `in` .
  * @param in               (multi-channel) signal to write to the buffer. Its
  *                         number of channels should match those of `buf` .
  * @param trig             a non-positive to positive transition causes the UGen
  *                         to append the current input values to the buffer
  * @param reset            a non-positive to positive transition causes the write
  *                         index into the buffer to be reset to zero. The contents
  *                         of the buffer will also be filled with zeroes. If the
  *                         buffer was full, the UGen output switches back to zero.
  * 
  * @see [[de.sciss.synth.ugen.ListTrig$ ListTrig]]
  */
final case class Logger(rate: Rate, buf: GE, in: GE, trig: GE, reset: GE = 0)
  extends UGenSource.SingleOut with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, trig.expand, reset.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, rate, _args, hasSideEffect = true, isIndividual = true)
}

/** A UGen that produces a scheduled sequences of trigger impulses. Trigger times
  * are provided as a list (buffer) of absolute offsets from time zero. A trigger is
  * output as a single control period of value `1` , after which output returns to
  * zero.
  * 
  * ===Examples===
  * 
  * {{{
  * // trigger grains
  * val b = Buffer(s)
  * b.alloc(10)
  * b.setData(Vector(1, 2, 3, 5, 8, 13, 21, 34, 55, 89).map(_ * 0.1f)) // quasi Fibonacci
  * 
  * val x = play {
  *   val reset = "reset".tr
  *   val tr  = ListTrig.kr(b.id, BufFrames.kr(b.id), reset)
  *   Timer.kr(tr).poll(tr, "timer")
  *   val env = EnvGen.ar(Env.perc(0.01, 0.1), gate = tr)
  *   SinOsc.ar(Seq(440, 460)) * env * 0.2
  * }
  * 
  * x.set("reset" -> 1)  // start anew
  * x.free(); b.free()
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.ListTrig2$ ListTrig2]]
  * @see [[de.sciss.synth.ugen.Logger$ Logger]]
  * @see [[de.sciss.synth.ugen.DemandEnvGen$ DemandEnvGen]]
  * @see [[de.sciss.synth.ugen.Dbufrd$ Dbufrd]]
  * @see [[de.sciss.synth.ugen.Timer$ Timer]]
  */
object ListTrig extends ProductReader[ListTrig] {
  /** @param buf              identifier of the buffer containing the offsets for the
    *                         triggers in seconds. The offsets are taken against the
    *                         start time of the synth or the last time a `reset` was
    *                         received. They are not accumulative, and the behavior is
    *                         undefined if the values are not sorted in ascending
    *                         order. The buffer should be monophonic.
    * @param size             the number of values to use from the buffer. Typically,
    *                         this should be `BufFrames.kr(buf)` .
    * @param reset            resets the timer and begins reading the time offsets
    *                         again at the start of the buffer.
    * @param delay            this value is added to each of the buffer values. For
    *                         example, to delay the list of values all by half a
    *                         second, use a `delay` of `0.5` . This parameter is only
    *                         updated at initialization or reset.
    */
  def kr(buf: GE, size: GE, reset: GE = 0, delay: GE = 0): ListTrig = 
    new ListTrig(control, buf, size, reset, delay)
  
  def read(in: RefMapIn, prefix: String, arity: Int): ListTrig = {
    require (arity == 5)
    val _rate   = in.readRate()
    val _buf    = in.readGE()
    val _size   = in.readGE()
    val _reset  = in.readGE()
    val _delay  = in.readGE()
    new ListTrig(_rate, _buf, _size, _reset, _delay)
  }
}

/** A UGen that produces a scheduled sequences of trigger impulses. Trigger times
  * are provided as a list (buffer) of absolute offsets from time zero. A trigger is
  * output as a single control period of value `1` , after which output returns to
  * zero.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param buf              identifier of the buffer containing the offsets for the
  *                         triggers in seconds. The offsets are taken against the
  *                         start time of the synth or the last time a `reset` was
  *                         received. They are not accumulative, and the behavior is
  *                         undefined if the values are not sorted in ascending
  *                         order. The buffer should be monophonic.
  * @param size             the number of values to use from the buffer. Typically,
  *                         this should be `BufFrames.kr(buf)` .
  * @param reset            resets the timer and begins reading the time offsets
  *                         again at the start of the buffer.
  * @param delay            this value is added to each of the buffer values. For
  *                         example, to delay the list of values all by half a
  *                         second, use a `delay` of `0.5` . This parameter is only
  *                         updated at initialization or reset.
  * 
  * @see [[de.sciss.synth.ugen.ListTrig2$ ListTrig2]]
  * @see [[de.sciss.synth.ugen.Logger$ Logger]]
  * @see [[de.sciss.synth.ugen.DemandEnvGen$ DemandEnvGen]]
  * @see [[de.sciss.synth.ugen.Dbufrd$ Dbufrd]]
  * @see [[de.sciss.synth.ugen.Timer$ Timer]]
  */
final case class ListTrig(rate: Rate, buf: GE, size: GE, reset: GE = 0, delay: GE = 0)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, reset.expand, delay.expand, size.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen that produces a scheduled sequences of trigger impulses. Trigger times
  * are provided as a list (buffer) of relative durations between consecutive
  * events. A trigger is output as a single control period of value `1` , after
  * which output returns to zero.
  * 
  * ===Examples===
  * 
  * {{{
  * // trigger grains
  * val b = Buffer(s)
  * b.alloc(11)
  * b.setData(Vector(1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89).map(_ * 0.1f)) // Fibonacci
  * 
  * val x = play {
  *   val reset = "reset".tr
  *   val tr  = ListTrig2.kr(b.id, BufFrames.kr(b.id), reset)
  *   Timer.kr(tr).poll(tr, "timer")
  *   val env = EnvGen.ar(Env.perc(0.01, 0.1), gate = tr)
  *   SinOsc.ar(Seq(440, 460)) * env * 0.2
  * }
  * 
  * x.set("reset" -> 1)  // start anew
  * x.free(); b.free()
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.ListTrig$ ListTrig]]
  * @see [[de.sciss.synth.ugen.Logger$ Logger]]
  * @see [[de.sciss.synth.ugen.DemandEnvGen$ DemandEnvGen]]
  * @see [[de.sciss.synth.ugen.Dbufrd$ Dbufrd]]
  * @see [[de.sciss.synth.ugen.ListTrig2$ ListTrig2]]
  */
object ListTrig2 extends ProductReader[ListTrig2] {
  /** @param buf              identifier of the buffer containing the durations for
    *                         the triggers in seconds. A value represents a relative
    *                         offsets with respect to its predecessor. The first value
    *                         indicates the time between the start of the synth or
    *                         last `reset` received and the first trigger. The buffer
    *                         should be monophonic.
    * @param size             the number of values to use from the buffer. Typically,
    *                         this should be `BufFrames.kr(buf)` .
    * @param reset            resets the timer and begins reading the time deltas
    *                         again at the start of the buffer.
    */
  def kr(buf: GE, size: GE, reset: GE = 0): ListTrig2 = new ListTrig2(control, buf, size, reset)
  
  def read(in: RefMapIn, prefix: String, arity: Int): ListTrig2 = {
    require (arity == 4)
    val _rate   = in.readRate()
    val _buf    = in.readGE()
    val _size   = in.readGE()
    val _reset  = in.readGE()
    new ListTrig2(_rate, _buf, _size, _reset)
  }
}

/** A UGen that produces a scheduled sequences of trigger impulses. Trigger times
  * are provided as a list (buffer) of relative durations between consecutive
  * events. A trigger is output as a single control period of value `1` , after
  * which output returns to zero.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param buf              identifier of the buffer containing the durations for
  *                         the triggers in seconds. A value represents a relative
  *                         offsets with respect to its predecessor. The first value
  *                         indicates the time between the start of the synth or
  *                         last `reset` received and the first trigger. The buffer
  *                         should be monophonic.
  * @param size             the number of values to use from the buffer. Typically,
  *                         this should be `BufFrames.kr(buf)` .
  * @param reset            resets the timer and begins reading the time deltas
  *                         again at the start of the buffer.
  * 
  * @see [[de.sciss.synth.ugen.ListTrig$ ListTrig]]
  * @see [[de.sciss.synth.ugen.Logger$ Logger]]
  * @see [[de.sciss.synth.ugen.DemandEnvGen$ DemandEnvGen]]
  * @see [[de.sciss.synth.ugen.Dbufrd$ Dbufrd]]
  * @see [[de.sciss.synth.ugen.ListTrig2$ ListTrig2]]
  */
final case class ListTrig2(rate: Rate, buf: GE, size: GE, reset: GE = 0)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, reset.expand, size.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen that finds the largest value in a buffer, providing both the value and
  * the index.
  * 
  * ===Examples===
  * 
  * {{{
  * // simple test
  * val b = Buffer(s)
  * b.alloc(100)
  * b.zero()
  * b.set(33 -> 1.034)
  * 
  * // verify that the 33rd value is detected...
  * val x = play {
  *   val m  = BufMax.kr(b.id)
  *   val tr = "poll".tr(1)
  *   m.value.poll(tr, "max-value")
  *   m.index.poll(tr, "max-index")
  *   ()
  * }
  * 
  * // ...until we set a new maximum...
  * b.set(74 -> 1.038); x.set("poll" -> 1)
  * 
  * x.free(); b.free()
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @see [[de.sciss.synth.ugen.BufMin$ BufMin]]
  * @see [[de.sciss.synth.ugen.ArrayMax$ ArrayMax]]
  */
object BufMax extends ProductReader[BufMax] {
  /** @param buf              identifier of the buffer containing the values to
    *                         analyze. It treats multi-channel buffers as monophonic,
    *                         and indices will refer to the de-interleaved frames and
    *                         channels.
    * @param gate             when closed (zero), holds the last output value.
    */
  def ir(buf: GE, gate: GE = 1): BufMax = new BufMax(scalar, buf, gate)
  
  /** @param buf              identifier of the buffer containing the values to
    *                         analyze. It treats multi-channel buffers as monophonic,
    *                         and indices will refer to the de-interleaved frames and
    *                         channels.
    * @param gate             when closed (zero), holds the last output value.
    */
  def kr(buf: GE, gate: GE = 1): BufMax = new BufMax(control, buf, gate)
  
  def read(in: RefMapIn, prefix: String, arity: Int): BufMax = {
    require (arity == 3)
    val _rate = in.readRate()
    val _buf  = in.readGE()
    val _gate = in.readGE()
    new BufMax(_rate, _buf, _gate)
  }
}

/** A UGen that finds the largest value in a buffer, providing both the value and
  * the index.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param buf              identifier of the buffer containing the values to
  *                         analyze. It treats multi-channel buffers as monophonic,
  *                         and indices will refer to the de-interleaved frames and
  *                         channels.
  * @param gate             when closed (zero), holds the last output value.
  * 
  * @see [[de.sciss.synth.ugen.BufMin$ BufMin]]
  * @see [[de.sciss.synth.ugen.ArrayMax$ ArrayMax]]
  */
final case class BufMax(rate: Rate, buf: GE, gate: GE = 1) extends UGenSource.MultiOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, gate.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args, isIndividual = true)
  
  def value: GE = ChannelProxy(this, 0)
  
  def index: GE = ChannelProxy(this, 1)
}

/** A UGen that finds the smallest value in a buffer, providing both the value and
  * the index.
  * 
  * ===Examples===
  * 
  * {{{
  * // simple test
  * val b = Buffer(s)
  * b.alloc(100)
  * b.zero()
  * b.set(33 -> -1.034)
  * 
  * // verify that the 33rd value is detected...
  * val x = play {
  *   val m  = BufMin.kr(b.id)
  *   val tr = "poll".tr(1)
  *   m.value.poll(tr, "min-value")
  *   m.index.poll(tr, "min-index")
  *   ()
  * }
  * 
  * // ...until we set a new minimum...
  * b.set(74 -> -1.038); x.set("poll" -> 1)
  * 
  * x.free(); b.free()
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @see [[de.sciss.synth.ugen.BufMax$ BufMax]]
  * @see [[de.sciss.synth.ugen.ArrayMin$ ArrayMin]]
  */
object BufMin extends ProductReader[BufMin] {
  /** @param buf              identifier of the buffer containing the values to
    *                         analyze. It treats multi-channel buffers as monophonic,
    *                         and indices will refer to the de-interleaved frames and
    *                         channels.
    * @param gate             when closed (zero), holds the last output value.
    */
  def ir(buf: GE, gate: GE = 1): BufMin = new BufMin(scalar, buf, gate)
  
  /** @param buf              identifier of the buffer containing the values to
    *                         analyze. It treats multi-channel buffers as monophonic,
    *                         and indices will refer to the de-interleaved frames and
    *                         channels.
    * @param gate             when closed (zero), holds the last output value.
    */
  def kr(buf: GE, gate: GE = 1): BufMin = new BufMin(control, buf, gate)
  
  def read(in: RefMapIn, prefix: String, arity: Int): BufMin = {
    require (arity == 3)
    val _rate = in.readRate()
    val _buf  = in.readGE()
    val _gate = in.readGE()
    new BufMin(_rate, _buf, _gate)
  }
}

/** A UGen that finds the smallest value in a buffer, providing both the value and
  * the index.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param buf              identifier of the buffer containing the values to
  *                         analyze. It treats multi-channel buffers as monophonic,
  *                         and indices will refer to the de-interleaved frames and
  *                         channels.
  * @param gate             when closed (zero), holds the last output value.
  * 
  * @see [[de.sciss.synth.ugen.BufMax$ BufMax]]
  * @see [[de.sciss.synth.ugen.ArrayMin$ ArrayMin]]
  */
final case class BufMin(rate: Rate, buf: GE, gate: GE = 1) extends UGenSource.MultiOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, gate.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args, isIndividual = true)
  
  def value: GE = ChannelProxy(this, 0)
  
  def index: GE = ChannelProxy(this, 1)
}

/** A UGen that finds the largest value across the channels of its input signal,
  * providing both the value and the index.
  * 
  * ===Examples===
  * 
  * {{{
  * // randomly changing array of three numbers
  * play {
  *   val tr  = Impulse.kr(1)
  *   val sig = Vector.fill(3)(TIRand.kr(0, 100, tr))
  *   sig.zipWithIndex.foreach { case (n, i) => n.poll(tr, s"sig[$i]") }
  *   val m   = ArrayMax.kr(sig)
  *   m.value.poll(tr, "max-value")
  *   m.index.poll(tr, "max-index")
  *   ()
  * }
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @see [[de.sciss.synth.ugen.ArrayMin$ ArrayMin]]
  * @see [[de.sciss.synth.ugen.BufMax$ BufMax]]
  */
object ArrayMax extends ProductReader[ArrayMax] {
  /** @param in               multi-channel signal to analyze
    */
  def ir(in: GE): ArrayMax = new ArrayMax(scalar, in)
  
  /** @param in               multi-channel signal to analyze
    */
  def kr(in: GE): ArrayMax = new ArrayMax(control, in)
  
  /** @param in               multi-channel signal to analyze
    */
  def ar(in: GE): ArrayMax = new ArrayMax(audio, in)
  
  def read(in: RefMapIn, prefix: String, arity: Int): ArrayMax = {
    require (arity == 2)
    val _rate = in.readRate()
    val _in   = in.readGE()
    new ArrayMax(_rate, _in)
  }
}

/** A UGen that finds the largest value across the channels of its input signal,
  * providing both the value and the index.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param in               multi-channel signal to analyze
  * 
  * @see [[de.sciss.synth.ugen.ArrayMin$ ArrayMin]]
  * @see [[de.sciss.synth.ugen.BufMax$ BufMax]]
  */
final case class ArrayMax(rate: Rate, in: GE) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, in.expand.outputs)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args)
  
  def value: GE = ChannelProxy(this, 0)
  
  def index: GE = ChannelProxy(this, 1)
}

/** A UGen that finds the smallest value across the channels of its input signal,
  * providing both the value and the index.
  * 
  * ===Examples===
  * 
  * {{{
  * // randomly changing array of three numbers
  * play {
  *   val tr  = Impulse.kr(1)
  *   val sig = Vector.fill(3)(TIRand.kr(0, 100, tr))
  *   sig.zipWithIndex.foreach { case (n, i) => n.poll(tr, s"sig[$i]") }
  *   val m   = ArrayMin.kr(sig)
  *   m.value.poll(tr, "min-value")
  *   m.index.poll(tr, "min-index")
  *   ()
  * }
  * }}}
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @see [[de.sciss.synth.ugen.ArrayMax$ ArrayMax]]
  * @see [[de.sciss.synth.ugen.BufMin$ BufMin]]
  */
object ArrayMin extends ProductReader[ArrayMin] {
  /** @param in               multi-channel signal to analyze
    */
  def ir(in: GE): ArrayMin = new ArrayMin(scalar, in)
  
  /** @param in               multi-channel signal to analyze
    */
  def kr(in: GE): ArrayMin = new ArrayMin(control, in)
  
  /** @param in               multi-channel signal to analyze
    */
  def ar(in: GE): ArrayMin = new ArrayMin(audio, in)
  
  def read(in: RefMapIn, prefix: String, arity: Int): ArrayMin = {
    require (arity == 2)
    val _rate = in.readRate()
    val _in   = in.readGE()
    new ArrayMin(_rate, _in)
  }
}

/** A UGen that finds the smallest value across the channels of its input signal,
  * providing both the value and the index.
  * 
  * This is a third-party UGen (MCLDUGens).
  * 
  * @param in               multi-channel signal to analyze
  * 
  * @see [[de.sciss.synth.ugen.ArrayMax$ ArrayMax]]
  * @see [[de.sciss.synth.ugen.BufMin$ BufMin]]
  */
final case class ArrayMin(rate: Rate, in: GE) extends UGenSource.MultiOut {
  protected def makeUGens: UGenInLike = unwrap(this, in.expand.outputs)
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args)
  
  def value: GE = ChannelProxy(this, 0)
  
  def index: GE = ChannelProxy(this, 1)
}