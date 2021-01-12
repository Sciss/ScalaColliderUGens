// revision: 9
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen which polls results from demand-rate ugens when receiving a trigger.
  * When there is a trigger at the `trig` input, a value is demanded from each ugen
  * in the `in` input and output. The unit generators in the list should be
  * demand-rate. When there is a trigger at the reset input, the demand rate ugens
  * in the list are reset.
  * 
  * Note: By design, a reset trigger only resets the demand ugens; it does not
  * reset the value at Demand's output. Demand continues to hold its value until the
  * next value is demanded, at which point its output value will be the first
  * expected item in the `in` argument.
  * 
  * Note: One demand-rate ugen represents a single stream of values, so that
  * embedding the same ugen twice calls this stream twice per demand, possibly
  * yielding different values. To embed the same sequence twice, either make sure
  * the ugen is demanded only once, or create two instances of the ugen.
  * 
  * '''Warning''': Demand currently seems to have problems with infinite sequences.
  * As a workaround use a very large length instead. E.g. instead of
  * `Dbrown(0, 1, inf)` use `Dbrown(0, 1, 0xFFFFFFFF)` !
  * 
  * '''Warning''': Demand seems to have a problem with initial triggers. For
  * example `Demand.kr(Impulse.kr(0), 1)` will have a spurious zero value output
  * first.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  * @see [[de.sciss.synth.ugen.TDuty$ TDuty]]
  */
object Demand extends ProductReader[Demand] {
  /** @param trig             trigger. Can be any signal. A trigger happens when the
    *                         signal changes from non-positive to positive.
    * @param in               a demand-rate signal (possibly multi-channel) which is
    *                         read at each trigger
    * @param reset            trigger. Resets the list of ugens (`in`) when triggered.
    */
  def kr(trig: GE, in: GE, reset: GE = 0): Demand = new Demand(control, trig, in, reset)
  
  /** @param trig             trigger. Can be any signal. A trigger happens when the
    *                         signal changes from non-positive to positive.
    * @param in               a demand-rate signal (possibly multi-channel) which is
    *                         read at each trigger
    * @param reset            trigger. Resets the list of ugens (`in`) when triggered.
    */
  def ar(trig: GE, in: GE, reset: GE = 0): Demand = new Demand(audio, trig, in, reset)
  
  def read(in: RefMapIn, key: String, arity: Int): Demand = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _trig   = in.readGE()
    val _in     = in.readGE()
    val _reset  = in.readGE()
    new Demand(_rate, _trig, _in, _reset)
  }
}

/** A UGen which polls results from demand-rate ugens when receiving a trigger.
  * When there is a trigger at the `trig` input, a value is demanded from each ugen
  * in the `in` input and output. The unit generators in the list should be
  * demand-rate. When there is a trigger at the reset input, the demand rate ugens
  * in the list are reset.
  * 
  * Note: By design, a reset trigger only resets the demand ugens; it does not
  * reset the value at Demand's output. Demand continues to hold its value until the
  * next value is demanded, at which point its output value will be the first
  * expected item in the `in` argument.
  * 
  * Note: One demand-rate ugen represents a single stream of values, so that
  * embedding the same ugen twice calls this stream twice per demand, possibly
  * yielding different values. To embed the same sequence twice, either make sure
  * the ugen is demanded only once, or create two instances of the ugen.
  * 
  * '''Warning''': Demand currently seems to have problems with infinite sequences.
  * As a workaround use a very large length instead. E.g. instead of
  * `Dbrown(0, 1, inf)` use `Dbrown(0, 1, 0xFFFFFFFF)` !
  * 
  * '''Warning''': Demand seems to have a problem with initial triggers. For
  * example `Demand.kr(Impulse.kr(0), 1)` will have a spurious zero value output
  * first.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param trig             trigger. Can be any signal. A trigger happens when the
  *                         signal changes from non-positive to positive.
  * @param in               a demand-rate signal (possibly multi-channel) which is
  *                         read at each trigger
  * @param reset            trigger. Resets the list of ugens (`in`) when triggered.
  * 
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  * @see [[de.sciss.synth.ugen.TDuty$ TDuty]]
  */
final case class Demand(rate: MaybeRate, trig: GE, in: GE, reset: GE = 0)
  extends UGenSource.MultiOut with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, reset.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRateT(_args, 0, _rate)
    UGen.MultiOut(name, _rate, Vector.fill(_args.size.-(2))(_rate), _args1)
  }
}

/** A UGen which polls results from demand-rate ugens in intervals specified by a
  * duration input. A value from the `level` ugen is demanded and output according
  * to a stream of duration values. When there is a trigger at the reset input, the
  * `level` and the `dur` input are reset.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.TDuty$ TDuty]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  */
object Duty extends ProductReader[Duty] {
  /** @param dur              the provider of time values. Can be a demand-rate ugen
    *                         or any signal. The next poll is acquired after the
    *                         previous duration.
    * @param level            a demand-rate ugen providing the output values.
    * @param reset            a trigger which resets the dur input (if demand-rated)
    *                         and the the level input ugen. The reset input may also
    *                         be a demand-rate ugen, in this case providing a stream
    *                         of reset times.
    * @param doneAction       a doneAction that is evaluated when the duration stream
    *                         ends.
    */
  def kr(dur: GE = 1.0f, level: GE, reset: GE = 0, doneAction: GE = doNothing): Duty = 
    new Duty(control, dur, level, reset, doneAction)
  
  /** @param dur              the provider of time values. Can be a demand-rate ugen
    *                         or any signal. The next poll is acquired after the
    *                         previous duration.
    * @param level            a demand-rate ugen providing the output values.
    * @param reset            a trigger which resets the dur input (if demand-rated)
    *                         and the the level input ugen. The reset input may also
    *                         be a demand-rate ugen, in this case providing a stream
    *                         of reset times.
    * @param doneAction       a doneAction that is evaluated when the duration stream
    *                         ends.
    */
  def ar(dur: GE = 1.0f, level: GE, reset: GE = 0, doneAction: GE = doNothing): Duty = 
    new Duty(audio, dur, level, reset, doneAction)
  
  def read(in: RefMapIn, key: String, arity: Int): Duty = {
    require (arity == 5)
    val _rate       = in.readRate()
    val _dur        = in.readGE()
    val _level      = in.readGE()
    val _reset      = in.readGE()
    val _doneAction = in.readGE()
    new Duty(_rate, _dur, _level, _reset, _doneAction)
  }
}

/** A UGen which polls results from demand-rate ugens in intervals specified by a
  * duration input. A value from the `level` ugen is demanded and output according
  * to a stream of duration values. When there is a trigger at the reset input, the
  * `level` and the `dur` input are reset.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param dur              the provider of time values. Can be a demand-rate ugen
  *                         or any signal. The next poll is acquired after the
  *                         previous duration.
  * @param level            a demand-rate ugen providing the output values.
  * @param reset            a trigger which resets the dur input (if demand-rated)
  *                         and the the level input ugen. The reset input may also
  *                         be a demand-rate ugen, in this case providing a stream
  *                         of reset times.
  * @param doneAction       a doneAction that is evaluated when the duration stream
  *                         ends.
  * 
  * @see [[de.sciss.synth.ugen.TDuty$ TDuty]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  */
final case class Duty(rate: Rate, dur: GE = 1.0f, level: GE, reset: GE = 0, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](dur.expand, reset.expand, doneAction.expand, level.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** A UGen which polls results from demand-rate ugens in intervals specified by a
  * duration input, and outputs them as trigger values. A value from the `level`
  * ugen is demanded and output for one sample (when running at audio-rate) or one
  * block (when running at control-rate) according to a stream of duration values.
  * When there is a trigger at the reset input, the `level` and the `dur` input are
  * reset.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  */
object TDuty extends ProductReader[TDuty] {
  def kr: TDuty = kr()
  
  /** @param dur              the provider of time values. Can be a demand-rate ugen
    *                         or any signal. The next poll is acquired after the
    *                         previous duration.
    * @param level            a demand-rate ugen providing the output values.
    * @param reset            a trigger which resets the dur input (if demand-rated)
    *                         and the the level input ugen. The reset input may also
    *                         be a demand-rate ugen, in this case providing a stream
    *                         of reset times.
    * @param doneAction       a doneAction that is evaluated when the duration stream
    *                         ends.
    * @param gapFirst         when 0 (default), the UGen does the first level poll
    *                         immediately and then waits for the first duration value.
    *                         When this is 1, the UGen initially polls the first
    *                         duration value, waits for that duration, and then polls
    *                         the first level (along with polling the next duration
    *                         value).
    */
  def kr(dur: GE = 1.0f, level: GE = 1.0f, reset: GE = 0, doneAction: GE = doNothing, gapFirst: GE = 0): TDuty = 
    new TDuty(control, dur, level, reset, doneAction, gapFirst)
  
  def ar: TDuty = ar()
  
  /** @param dur              the provider of time values. Can be a demand-rate ugen
    *                         or any signal. The next poll is acquired after the
    *                         previous duration.
    * @param level            a demand-rate ugen providing the output values.
    * @param reset            a trigger which resets the dur input (if demand-rated)
    *                         and the the level input ugen. The reset input may also
    *                         be a demand-rate ugen, in this case providing a stream
    *                         of reset times.
    * @param doneAction       a doneAction that is evaluated when the duration stream
    *                         ends.
    * @param gapFirst         when 0 (default), the UGen does the first level poll
    *                         immediately and then waits for the first duration value.
    *                         When this is 1, the UGen initially polls the first
    *                         duration value, waits for that duration, and then polls
    *                         the first level (along with polling the next duration
    *                         value).
    */
  def ar(dur: GE = 1.0f, level: GE = 1.0f, reset: GE = 0, doneAction: GE = doNothing, gapFirst: GE = 0): TDuty = 
    new TDuty(audio, dur, level, reset, doneAction, gapFirst)
  
  def read(in: RefMapIn, key: String, arity: Int): TDuty = {
    require (arity == 6)
    val _rate       = in.readRate()
    val _dur        = in.readGE()
    val _level      = in.readGE()
    val _reset      = in.readGE()
    val _doneAction = in.readGE()
    val _gapFirst   = in.readGE()
    new TDuty(_rate, _dur, _level, _reset, _doneAction, _gapFirst)
  }
}

/** A UGen which polls results from demand-rate ugens in intervals specified by a
  * duration input, and outputs them as trigger values. A value from the `level`
  * ugen is demanded and output for one sample (when running at audio-rate) or one
  * block (when running at control-rate) according to a stream of duration values.
  * When there is a trigger at the reset input, the `level` and the `dur` input are
  * reset.
  * 
  * @note The argument order is different from its sclang counterpart.
  * 
  * @param dur              the provider of time values. Can be a demand-rate ugen
  *                         or any signal. The next poll is acquired after the
  *                         previous duration.
  * @param level            a demand-rate ugen providing the output values.
  * @param reset            a trigger which resets the dur input (if demand-rated)
  *                         and the the level input ugen. The reset input may also
  *                         be a demand-rate ugen, in this case providing a stream
  *                         of reset times.
  * @param doneAction       a doneAction that is evaluated when the duration stream
  *                         ends.
  * @param gapFirst         when 0 (default), the UGen does the first level poll
  *                         immediately and then waits for the first duration value.
  *                         When this is 1, the UGen initially polls the first
  *                         duration value, waits for that duration, and then polls
  *                         the first level (along with polling the next duration
  *                         value).
  * 
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  */
final case class TDuty(rate: Rate, dur: GE = 1.0f, level: GE = 1.0f, reset: GE = 0, doneAction: GE = doNothing, gapFirst: GE = 0)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](dur.expand, reset.expand, doneAction.expand, level.expand, gapFirst.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** An envelope generator UGen using demand-rate inputs for the envelope segments.
  * For each parameter of the envelope (levels, durations and shapes), values are
  * polled every time a new segment starts.
  * 
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  * @see [[de.sciss.synth.ugen.Env$ Env]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  */
object DemandEnvGen extends ProductReader[DemandEnvGen] {
  /** @param levels           demand-rate ugen (or other ugen) returning level values
    * @param durs             demand-rate ugen (or other ugen) returning duration
    *                         values
    * @param shapes           demand-rate ugen (or other ugen) returning shape number
    *                         for the envelope segment.
    * @param curvatures       demand-rate ugen (or other ugen) returning curvature
    *                         values. these are used for curveShape segments (shape
    *                         number 5) and should be zero for other shapes.
    * @param gate             a control rate gate: if gate is x >= 1, the ugen runs.
    *                         if gate is 0 > x > 1, the ugen is released at the next
    *                         level (according to doneAction). if gate is x <= 0, the
    *                         ugen is sampled and held.
    * @param reset            a trigger signal. a trigger occurs when passing from
    *                         non-positive to positive. when the trigger amplitude is
    *                         < 1, the input ugens (those that are demand-rated) are
    *                         reset when the current segment ends. if the trigger
    *                         amplitude is > 1, the reset is performed immediately.
    * @param levelScale       demand-rate ugen returning level scaling values
    * @param levelBias        demand-rate ugen returning level offset values
    * @param timeScale        demand-rate ugen returning time scaling values
    * @param doneAction       a done action performed when one of the demand-rated
    *                         series ends
    */
  def ar(levels: GE, durs: GE, shapes: GE = 1, curvatures: GE = 0.0f, gate: GE = 1.0f, reset: GE = 1.0f, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f, doneAction: GE = doNothing): DemandEnvGen = 
    new DemandEnvGen(audio, levels, durs, shapes, curvatures, gate, reset, levelScale, levelBias, timeScale, doneAction)
  
  def read(in: RefMapIn, key: String, arity: Int): DemandEnvGen = {
    require (arity == 11)
    val _rate       = in.readRate()
    val _levels     = in.readGE()
    val _durs       = in.readGE()
    val _shapes     = in.readGE()
    val _curvatures = in.readGE()
    val _gate       = in.readGE()
    val _reset      = in.readGE()
    val _levelScale = in.readGE()
    val _levelBias  = in.readGE()
    val _timeScale  = in.readGE()
    val _doneAction = in.readGE()
    new DemandEnvGen(_rate, _levels, _durs, _shapes, _curvatures, _gate, _reset, _levelScale, _levelBias, _timeScale, _doneAction)
  }
}

/** An envelope generator UGen using demand-rate inputs for the envelope segments.
  * For each parameter of the envelope (levels, durations and shapes), values are
  * polled every time a new segment starts.
  * 
  * @param levels           demand-rate ugen (or other ugen) returning level values
  * @param durs             demand-rate ugen (or other ugen) returning duration
  *                         values
  * @param shapes           demand-rate ugen (or other ugen) returning shape number
  *                         for the envelope segment.
  * @param curvatures       demand-rate ugen (or other ugen) returning curvature
  *                         values. these are used for curveShape segments (shape
  *                         number 5) and should be zero for other shapes.
  * @param gate             a control rate gate: if gate is x >= 1, the ugen runs.
  *                         if gate is 0 > x > 1, the ugen is released at the next
  *                         level (according to doneAction). if gate is x <= 0, the
  *                         ugen is sampled and held.
  * @param reset            a trigger signal. a trigger occurs when passing from
  *                         non-positive to positive. when the trigger amplitude is
  *                         < 1, the input ugens (those that are demand-rated) are
  *                         reset when the current segment ends. if the trigger
  *                         amplitude is > 1, the reset is performed immediately.
  * @param levelScale       demand-rate ugen returning level scaling values
  * @param levelBias        demand-rate ugen returning level offset values
  * @param timeScale        demand-rate ugen returning time scaling values
  * @param doneAction       a done action performed when one of the demand-rated
  *                         series ends
  * 
  * @see [[de.sciss.synth.ugen.EnvGen$ EnvGen]]
  * @see [[de.sciss.synth.ugen.Env$ Env]]
  * @see [[de.sciss.synth.DoneAction DoneAction]]
  */
final case class DemandEnvGen(rate: Rate, levels: GE, durs: GE, shapes: GE = 1, curvatures: GE = 0.0f, gate: GE = 1.0f, reset: GE = 1.0f, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f, doneAction: GE = doNothing)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](levels.expand, durs.expand, shapes.expand, curvatures.expand, gate.expand, reset.expand, levelScale.expand, levelBias.expand, timeScale.expand, doneAction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, hasSideEffect = true)
}

/** A demand-rate UGen which produces an arithmetic (linear) series.
  * 
  * The arguments can be constant or any other ugens.
  * 
  * @see [[de.sciss.synth.ugen.Dgeom$ Dgeom]]
  * @see [[de.sciss.synth.ugen.Dseq$ Dseq]]
  */
object Dseries extends ProductReader[Dseries] {
  def read(in: RefMapIn, key: String, arity: Int): Dseries = {
    require (arity == 3)
    val _start  = in.readGE()
    val _step   = in.readGE()
    val _length = in.readGE()
    new Dseries(_start, _step, _length)
  }
}

/** A demand-rate UGen which produces an arithmetic (linear) series.
  * 
  * The arguments can be constant or any other ugens.
  * 
  * @param start            the start value of the series
  * @param step             the incremental step by which the series changes. the
  *                         step is added to the previous value on each demand.
  * @param length           the number of elements to produces (maybe be infinite)
  * 
  * @see [[de.sciss.synth.ugen.Dgeom$ Dgeom]]
  * @see [[de.sciss.synth.ugen.Dseq$ Dseq]]
  */
final case class Dseries(start: GE = 0.0f, step: GE = 1.0f, length: GE = inf)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand, start.expand, step.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that produces a geometric series. Each value is calculated as
  * {{{
  * x[t] = x[t-1] * grow
  * }}}
  * 
  * With `x[0]` given as argument `start`.
  * 
  * ===Examples===
  * 
  * {{{
  * // increasing frequency
  * play {
  *   val in = Dgeom(1, 1.2, 15)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = v * 30 + 300
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Dseries$ Dseries]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object Dgeom extends ProductReader[Dgeom] {
  def read(in: RefMapIn, key: String, arity: Int): Dgeom = {
    require (arity == 3)
    val _start  = in.readGE()
    val _grow   = in.readGE()
    val _length = in.readGE()
    new Dgeom(_start, _grow, _length)
  }
}

/** A demand-rate UGen that produces a geometric series. Each value is calculated as
  * {{{
  * x[t] = x[t-1] * grow
  * }}}
  * 
  * With `x[0]` given as argument `start`.
  * 
  * @param start            initial value
  * @param grow             factor (multiplier) by which values grow
  * @param length           length of the sequence or `inf`
  * 
  * @see [[de.sciss.synth.ugen.Dseries$ Dseries]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class Dgeom(start: GE = 1.0f, grow: GE = 2.0f, length: GE = inf)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand, start.expand, grow.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that produces random decimal numbers with a constant
  * spectral density, equivalent to `WhiteNoise` for audio- or control-rate.
  * 
  * ===Examples===
  * 
  * {{{
  * // random frequency
  * play {
  *   val in = Dwhite(lo = 0, hi = 15)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = v * 30 + 300
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Diwhite$ Diwhite]]
  * @see [[de.sciss.synth.ugen.Dbrown$ Dbrown]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object Dwhite extends ProductReader[Dwhite] {
  def read(in: RefMapIn, key: String, arity: Int): Dwhite = {
    require (arity == 3)
    val _lo     = in.readGE()
    val _hi     = in.readGE()
    val _length = in.readGE()
    new Dwhite(_lo, _hi, _length)
  }
}

/** A demand-rate UGen that produces random decimal numbers with a constant
  * spectral density, equivalent to `WhiteNoise` for audio- or control-rate.
  * 
  * @param lo               minimum value (inclusive)
  * @param hi               maximum value (inclusive)
  * @param length           length of the sequence or `inf`
  * 
  * @see [[de.sciss.synth.ugen.Diwhite$ Diwhite]]
  * @see [[de.sciss.synth.ugen.Dbrown$ Dbrown]]
  * @see [[de.sciss.synth.ugen.WhiteNoise$ WhiteNoise]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class Dwhite(lo: GE = 0.0f, hi: GE = 1.0f, length: GE = inf)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that produces equally distributed random integer numbers.
  * 
  * ===Examples===
  * 
  * {{{
  * // random pitch
  * play {
  *   val in = Diwhite(lo = 0, hi = 15)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Dwhite$ Dwhite]]
  * @see [[de.sciss.synth.ugen.Dibrown$ Dibrown]]
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TIRand$ TIRand]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object Diwhite extends ProductReader[Diwhite] {
  def read(in: RefMapIn, key: String, arity: Int): Diwhite = {
    require (arity == 3)
    val _lo     = in.readGE()
    val _hi     = in.readGE()
    val _length = in.readGE()
    new Diwhite(_lo, _hi, _length)
  }
}

/** A demand-rate UGen that produces equally distributed random integer numbers.
  * 
  * @param lo               minimum value (inclusive)
  * @param hi               maximum value (inclusive)
  * @param length           length of the sequence or `inf`
  * 
  * @see [[de.sciss.synth.ugen.Dwhite$ Dwhite]]
  * @see [[de.sciss.synth.ugen.Dibrown$ Dibrown]]
  * @see [[de.sciss.synth.ugen.IRand$ IRand]]
  * @see [[de.sciss.synth.ugen.TIRand$ TIRand]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class Diwhite(lo: GE = 0, hi: GE = 1, length: GE = inf)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand, lo.expand, hi.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that produces random decimal numbers, analogous to a
  * Brownian motion.
  * 
  * ===Examples===
  * 
  * {{{
  * // random frequency
  * play {
  *   val in = Dbrown(lo = 0, hi = 15, step = 1)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = v * 30 + 300
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Dwhite$ Dwhite]]
  * @see [[de.sciss.synth.ugen.Dibrown$ Dibrown]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object Dbrown extends ProductReader[Dbrown] {
  def read(in: RefMapIn, key: String, arity: Int): Dbrown = {
    require (arity == 4)
    val _lo     = in.readGE()
    val _hi     = in.readGE()
    val _step   = in.readGE()
    val _length = in.readGE()
    new Dbrown(_lo, _hi, _step, _length)
  }
}

/** A demand-rate UGen that produces random decimal numbers, analogous to a
  * Brownian motion.
  * 
  * @param lo               minimum value (inclusive)
  * @param hi               minimum value (inclusive)
  * @param step             the maximum step a value can take from the previous
  *                         value
  * @param length           length of the sequence or `inf`
  * 
  * @see [[de.sciss.synth.ugen.Dwhite$ Dwhite]]
  * @see [[de.sciss.synth.ugen.Dibrown$ Dibrown]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class Dbrown(lo: GE = 0.0f, hi: GE = 1.0f, step: GE = 0.01f, length: GE = inf)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](length.expand, lo.expand, hi.expand, step.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that produces random integer numbers, analogous to a
  * Brownian motion, or the `drunk` object in Max.
  * 
  * '''Note''': The `length` parameter seems currently broken.
  * 
  * ===Examples===
  * 
  * {{{
  * // random frequency
  * play {
  *   val in = Dibrown(lo = 0, hi = 15, step = 2)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Diwhite$ Diwhite]]
  * @see [[de.sciss.synth.ugen.Dbrown$ Dbrown]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
object Dibrown extends ProductReader[Dibrown] {
  def read(in: RefMapIn, key: String, arity: Int): Dibrown = {
    require (arity == 4)
    val _lo     = in.readGE()
    val _hi     = in.readGE()
    val _step   = in.readGE()
    val _length = in.readGE()
    new Dibrown(_lo, _hi, _step, _length)
  }
}

/** A demand-rate UGen that produces random integer numbers, analogous to a
  * Brownian motion, or the `drunk` object in Max.
  * 
  * '''Note''': The `length` parameter seems currently broken.
  * 
  * @param lo               minimum value (inclusive)
  * @param hi               maximum value (inclusive)
  * @param step             the maximum step a value can take from the previous
  *                         value
  * @param length           length of the sequence or `inf` . This is currently
  *                         (SuperCollider 3.7.2) broken, and the sequence always
  *                         has infinite length.
  * 
  * @see [[de.sciss.synth.ugen.Diwhite$ Diwhite]]
  * @see [[de.sciss.synth.ugen.Dbrown$ Dbrown]]
  * @see [[de.sciss.synth.ugen.BrownNoise$ BrownNoise]]
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  */
final case class Dibrown(lo: GE = 0, hi: GE = 1, step: GE = 1, length: GE = inf)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](length.expand, lo.expand, hi.expand, step.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen which reproduces and repeats a sequence of values.
  * 
  * The arguments can be constant or any other ugens, where demand rate UGens are
  * normally polled, proceeding to the next value when the sub-sequence ends.
  * 
  * ===Examples===
  * 
  * {{{
  * // sequence of constants
  * play {
  *   val in = Dseq(Seq(3, 5, 7, 8, 11), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * {{{
  * // constants alternating with white noise samples
  * play {
  *   val in = Dseq(Seq[GE](3, 5, 7, 8, 11, WhiteNoise.kr * 2), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * {{{
  * // constants alternating with two brownian values
  * play {
  *   val br = Dbrown(lo = -3, hi = 3, step = 1, length = 2)
  *   val c7 = Dseq(7, repeats = inf)
  *   val in = Dseq(Seq[GE](3, 5, 7, 8, 11, br), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  */
object Dseq extends ProductReader[Dseq] {
  def read(in: RefMapIn, key: String, arity: Int): Dseq = {
    require (arity == 2)
    val _seq      = in.readGE()
    val _repeats  = in.readGE()
    new Dseq(_seq, _repeats)
  }
}

/** A demand-rate UGen which reproduces and repeats a sequence of values.
  * 
  * The arguments can be constant or any other ugens, where demand rate UGens are
  * normally polled, proceeding to the next value when the sub-sequence ends.
  * 
  * @param seq              A multi-channel signal providing the values at each
  *                         step of the sequence. Typically (but not necessarily) a
  *                         sequence of constant values.
  * @param repeats          The number of repetitions of the entire sequence
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  */
final case class Dseq(seq: GE, repeats: GE = 1)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](repeats.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen which reproduces and repeats values of a sequence. It is
  * very similar to `Dseq` , the difference being that the `length` parameter
  * specifies the length of output sequence, not the number of repetitions of the
  * input sequence. The input sequence is repeated if `length` is greater than the
  * length of the input sequence.
  * 
  * The arguments can be constant or any other ugens, where demand rate UGens are
  * normally polled, proceeding to the next value when the sub-sequence ends.
  * 
  * ===Examples===
  * 
  * {{{
  * // sequence of constants
  * play {
  *   val in = Dseq(Seq(3, 5, 7, 8, 11), 8)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dseq$ Dseq]]
  */
object Dser extends ProductReader[Dser] {
  def read(in: RefMapIn, key: String, arity: Int): Dser = {
    require (arity == 2)
    val _seq    = in.readGE()
    val _length = in.readGE()
    new Dser(_seq, _length)
  }
}

/** A demand-rate UGen which reproduces and repeats values of a sequence. It is
  * very similar to `Dseq` , the difference being that the `length` parameter
  * specifies the length of output sequence, not the number of repetitions of the
  * input sequence. The input sequence is repeated if `length` is greater than the
  * length of the input sequence.
  * 
  * The arguments can be constant or any other ugens, where demand rate UGens are
  * normally polled, proceeding to the next value when the sub-sequence ends.
  * 
  * @param seq              A multi-channel signal providing the values at each
  *                         step of the sequence. Typically (but not necessarily) a
  *                         sequence of constant values.
  * @param length           The number of items polled from the sequence
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dseq$ Dseq]]
  */
final case class Dser(seq: GE, length: GE = 1)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that reads out a buffer. All inputs can be either demand
  * UGens or any other UGens.
  * 
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.Dbufwr$ Dbufwr]]
  */
object Dbufrd extends ProductReader[Dbufrd] {
  def read(in: RefMapIn, key: String, arity: Int): Dbufrd = {
    require (arity == 3)
    val _buf    = in.readGE()
    val _index  = in.readGE()
    val _loop   = in.readGE()
    new Dbufrd(_buf, _index, _loop)
  }
}

/** A demand-rate UGen that reads out a buffer. All inputs can be either demand
  * UGens or any other UGens.
  * 
  * @param buf              the identifier of the buffer to read out
  * @param index            the frame index into the buffer
  * @param loop             whether to wrap an exceeding phase around the buffer
  *                         length (1) or not (0)
  * 
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.Dbufwr$ Dbufwr]]
  */
final case class Dbufrd(buf: GE, index: GE = 0, loop: GE = 1)
  extends UGenSource.SingleOut with DemandRated with IsIndividual with HasDoneFlag {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, index.expand, loop.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}
object Dbufwr extends ProductReader[Dbufwr] {
  def read(in: RefMapIn, key: String, arity: Int): Dbufwr = {
    require (arity == 4)
    val _in     = in.readGE()
    val _buf    = in.readGE()
    val _index  = in.readGE()
    val _loop   = in.readGE()
    new Dbufwr(_in, _buf, _index, _loop)
  }
}
final case class Dbufwr(in: GE, buf: GE, index: GE = 0, loop: GE = 1)
  extends UGenSource.SingleOut with DemandRated with HasSideEffect with IsIndividual with HasDoneFlag {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, index.expand, in.expand, loop.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, demand, _args, hasSideEffect = true, isIndividual = true)
}

/** A demand-rate UGen which outputs random elements from a sequence. `Drand`
  * allows duplicate elements to appear, whereas `Dxrand` forbids repetitions.
  * 
  * ===Examples===
  * 
  * {{{
  * // sequence of constants
  * play {
  *   val in = Drand(Seq(3, 5, 7, 8, 11), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  * @see [[de.sciss.synth.ugen.Dxrand$ Dxrand]]
  * @see [[de.sciss.synth.ugen.Diwhite$ Diwhite]]
  */
object Drand extends ProductReader[Drand] {
  def read(in: RefMapIn, key: String, arity: Int): Drand = {
    require (arity == 2)
    val _seq    = in.readGE()
    val _length = in.readGE()
    new Drand(_seq, _length)
  }
}

/** A demand-rate UGen which outputs random elements from a sequence. `Drand`
  * allows duplicate elements to appear, whereas `Dxrand` forbids repetitions.
  * 
  * @param seq              A multi-channel signal providing the values at each
  *                         step of the sequence. Typically (but not necessarily) a
  *                         sequence of constant values.
  * @param length           The number of items polled from the sequence
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  * @see [[de.sciss.synth.ugen.Dxrand$ Dxrand]]
  * @see [[de.sciss.synth.ugen.Diwhite$ Diwhite]]
  */
final case class Drand(seq: GE, length: GE = 1)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen which outputs random elements from a sequence. `Dxrand`
  * forbid repetitions, whereas `Drand` allows duplicate elements to appear.
  * 
  * ===Examples===
  * 
  * {{{
  * // sequence of constants
  * play {
  *   val in = Dxrand(Seq(3, 5, 7, 8, 11), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  * @see [[de.sciss.synth.ugen.Drand$ Drand]]
  * @see [[de.sciss.synth.ugen.Dshuf$ Dshuf]]
  */
object Dxrand extends ProductReader[Dxrand] {
  def read(in: RefMapIn, key: String, arity: Int): Dxrand = {
    require (arity == 2)
    val _seq    = in.readGE()
    val _length = in.readGE()
    new Dxrand(_seq, _length)
  }
}

/** A demand-rate UGen which outputs random elements from a sequence. `Dxrand`
  * forbid repetitions, whereas `Drand` allows duplicate elements to appear.
  * 
  * @param seq              A multi-channel signal providing the values at each
  *                         step of the sequence. Typically (but not necessarily) a
  *                         sequence of constant values.
  * @param length           The number of items polled from the sequence
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  * @see [[de.sciss.synth.ugen.Drand$ Drand]]
  * @see [[de.sciss.synth.ugen.Dshuf$ Dshuf]]
  */
final case class Dxrand(seq: GE, length: GE = 1)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](length.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen which outputs a randomly shuffled version of an input
  * sequence. The sequence is only shuffled once, thus when `repeats` is greater
  * than one, the same randomized sequence will be repeated, until the UGen is
  * reset. This happens for example, when it is nested inside a `Dseq` , as shown in
  * the example "reshuffle".
  * 
  * ===Examples===
  * 
  * {{{
  * // sequence of constants
  * play {
  *   val in = Dshuf(Seq(3, 5, 7, 8, 11), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * {{{
  * // reshuffle
  * play {
  *   val in = Dseq(Seq[GE](Dshuf(Seq(3, 5, 7, 8, 11), 4), 24), inf)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = (v + 62).midiCps
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * {{{
  * // audio rate
  * play {
  *   val in = Dseq(Seq(Dshuf(Seq.fill(81)(util.Random.nextInt(10)), 5)), inf)
  *   val tr = Impulse.ar(500)
  *   val f  = Demand.ar(tr, in) * 30 + 340
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * {{{
  * // embedded structures
  * play {
  *   val in = Dseq(repeats = inf, seq =
  *     Dshuf(repeats = 8, seq = Seq[GE](
  *       Drand(Seq(1, 2, 3), 1),
  *       3,
  *       Drand(Seq(20, 23, 56), 1),
  *       7, 8.5)))
  *   val tr = Impulse.kr(MouseX.kr(1, 40, 1))
  *   val f  = Demand.kr(tr, in) * 30 + 340
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  * @see [[de.sciss.synth.ugen.Drand$ Drand]]
  * @see [[de.sciss.synth.ugen.Dshuf$ Dshuf]]
  */
object Dshuf extends ProductReader[Dshuf] {
  def read(in: RefMapIn, key: String, arity: Int): Dshuf = {
    require (arity == 2)
    val _seq      = in.readGE()
    val _repeats  = in.readGE()
    new Dshuf(_seq, _repeats)
  }
}

/** A demand-rate UGen which outputs a randomly shuffled version of an input
  * sequence. The sequence is only shuffled once, thus when `repeats` is greater
  * than one, the same randomized sequence will be repeated, until the UGen is
  * reset. This happens for example, when it is nested inside a `Dseq` , as shown in
  * the example "reshuffle".
  * 
  * @param seq              A multi-channel signal providing the values at each
  *                         step of the sequence. Typically (but not necessarily) a
  *                         sequence of constant values.
  * @param repeats          The number of repetitions of the entire sequence
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Dser$ Dser]]
  * @see [[de.sciss.synth.ugen.Drand$ Drand]]
  * @see [[de.sciss.synth.ugen.Dshuf$ Dshuf]]
  */
final case class Dshuf(seq: GE, repeats: GE = 1)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](repeats.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}
object Dswitch1 extends ProductReader[Dswitch1] {
  def read(in: RefMapIn, key: String, arity: Int): Dswitch1 = {
    require (arity == 2)
    val _seq    = in.readGE()
    val _index  = in.readGE()
    new Dswitch1(_seq, _index)
  }
}
final case class Dswitch1(seq: GE, index: GE)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](index.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}
object Dswitch extends ProductReader[Dswitch] {
  def read(in: RefMapIn, key: String, arity: Int): Dswitch = {
    require (arity == 2)
    val _seq    = in.readGE()
    val _index  = in.readGE()
    new Dswitch(_seq, _index)
  }
}
final case class Dswitch(seq: GE, index: GE)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](index.expand).++(seq.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}
object Dstutter extends ProductReader[Dstutter] {
  def read(in: RefMapIn, key: String, arity: Int): Dstutter = {
    require (arity == 2)
    val _n  = in.readGE()
    val _in = in.readGE()
    new Dstutter(_n, _in)
  }
}
final case class Dstutter(n: GE, in: GE) extends UGenSource.SingleOut with DemandRated with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](n.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that outputs values from the child demand stream until the
  * sum of those values reaches or exceeds a given total. The last value will be
  * truncated so that the sum of `Dconst` 's output values will match the total
  * exactly.
  */
object Dconst extends ProductReader[Dconst] {
  def read(in: RefMapIn, key: String, arity: Int): Dconst = {
    require (arity == 3)
    val _sum        = in.readGE()
    val _in         = in.readGE()
    val _tolerance  = in.readGE()
    new Dconst(_sum, _in, _tolerance)
  }
}

/** A demand-rate UGen that outputs values from the child demand stream until the
  * sum of those values reaches or exceeds a given total. The last value will be
  * truncated so that the sum of `Dconst` 's output values will match the total
  * exactly.
  * 
  * @param sum              the sum to reach. This may be a number, demand UGen or
  *                         any other UGen. When a `Dconst` instance resets, one
  *                         value will be taken for the sum, and it can't be
  *                         modulated until the next reset.
  * @param in               a demand-rate stream, providing the output values
  * @param tolerance        how close the running sum can get to stop the output:
  *                         abs(running-sum - sum) <= tolerance. This is to avoid
  *                         the problem of floating point rounding error preventing
  *                         reaching the exact target sum.
  */
final case class Dconst(sum: GE, in: GE, tolerance: GE = 0.001f)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](sum.expand, in.expand, tolerance.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}
object Donce extends ProductReader[Donce] {
  def read(in: RefMapIn, key: String, arity: Int): Donce = {
    require (arity == 1)
    val _in = in.readGE()
    new Donce(_in)
  }
}
final case class Donce(in: GE) extends UGenSource.SingleOut with DemandRated with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand-rate UGen that, when triggered, resets the state of its `in` argument.
  * 
  * ===Examples===
  * 
  * {{{
  * // reset on mouse-click
  * play {
  *   val in = Dreset(Dseries(0, 2, inf), MouseButton.kr)
  *   val tr = Impulse.kr(5)
  *   val v  = Demand.kr(tr, in)
  *   v.poll(tr)
  *   val f  = v * 30 + 340
  *   SinOsc.ar(f) * 0.1
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  */
object Dreset extends ProductReader[Dreset] {
  def read(in: RefMapIn, key: String, arity: Int): Dreset = {
    require (arity == 2)
    val _in     = in.readGE()
    val _reset  = in.readGE()
    new Dreset(_in, _reset)
  }
}

/** A demand-rate UGen that, when triggered, resets the state of its `in` argument.
  * 
  * @param in               a demand-rate UGen, producing values which are passed
  *                         through, and which will be reset by this UGen
  * @param reset            a demand-rate or any other UGen. When crossing from
  *                         non-positive to positive, `Dreset` resets its `in`
  *                         argument.
  * 
  * @see [[de.sciss.synth.ugen.Demand$ Demand]]
  * @see [[de.sciss.synth.ugen.Duty$ Duty]]
  */
final case class Dreset(in: GE, reset: GE) extends UGenSource.SingleOut with DemandRated with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, reset.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A demand rate UGen printing the current output value of its input to the
  * console when polled.
  * 
  * @see [[de.sciss.synth.ugen.SendTrig$ SendTrig]]
  * @see [[de.sciss.synth.ugen.Poll$ Poll]]
  */
object Dpoll extends ProductReader[Dpoll] {
  def read(in: RefMapIn, key: String, arity: Int): Dpoll = {
    require (arity == 4)
    val _in     = in.readGE()
    val _label  = in.readString()
    val _run    = in.readGE()
    val _trigId = in.readGE()
    new Dpoll(_in, _label, _run, _trigId)
  }
}

/** A demand rate UGen printing the current output value of its input to the
  * console when polled.
  * 
  * @param in               the signal you want to poll
  * @param label            a string or symbol to be printed with the polled value
  *                         ''(init-time only)''
  * @param run              if 1 the polling is active, if 0 it is inactive.
  * @param trigId           if greater then 0, a `"/tr"` OSC message is sent back
  *                         to the client (similar to `SendTrig` )
  * 
  * @see [[de.sciss.synth.ugen.SendTrig$ SendTrig]]
  * @see [[de.sciss.synth.ugen.Poll$ Poll]]
  */
final case class Dpoll(in: GE, label: String = "poll", run: GE = 1, trigId: GE = -1)
  extends UGenSource.SingleOut with DemandRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, trigId.expand, run.expand).++(stringArg(label)))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, demand, _args, hasSideEffect = true, isIndividual = true)
}