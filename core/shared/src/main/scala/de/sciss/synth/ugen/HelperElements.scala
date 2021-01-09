/*
 *  HelperElements.scala
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
package ugen

import de.sciss.synth.GEOps._
import de.sciss.synth.UGenSource._
import de.sciss.synth.ugen.ControlProxyFactory._

import scala.annotation.{switch, tailrec}

object Flatten extends ProductReader[Flatten] {
  override def read(in: RefMapIn, prefix: String, arity: Int): Flatten = {
    require (arity == 1)
    val _elem = in.readGE()
    new Flatten(_elem)
  }
}
/** A graph element that flattens the channels from a nested multi-channel structure.
  *
  * @param elem the element to flatten
  */
final case class Flatten(elem: GE) extends GE.Lazy {
  def rate: MaybeRate = elem.rate

  override def toString = s"$elem.flatten"

  protected def makeUGens: UGenInLike = UGenInGroup(elem.expand.flatOutputs)
}

/** A graph element that mixes the channels of a signal together.
  * It works like the sclang counterpart.
  *
  * The `Mix` companion object contains various useful mixing idioms:
  *
  * - `Mix.tabulate(n: Int)(fun: Int => GE)`: corresponds to `Seq.tabulate`
  *   and to `Array.fill` in sclang.
  * - `Mix.fill(n: Int)(thunk: => GE)`: corresponds to `Seq.fill`.
  * - `Mix.seq(elems: GE*)`: A shortcut for `Mix(GESeq(elems: _*))`.
  *
  * A separate graph element is `Mix.mono`.
  * `Mix.mono(elem: GE)` flattens all channels of
  * the input element before summing them, guaranteeing that the result is monophonic.
  *
  * Finally, `Mix.fold` is an idiom that not actually adds elements,
  * but recursively folds them. Thus,
  * `Mix.fold(elem: GE, n: Int)(fun: GE => GE)` is equivalent to
  * {{{
  * (1 to n).foldLeft(elem) { (res, _) => fun(res) }
  * }}}
  *
  * `Mix.fold` is often used in the SuperCollider examples to apply a filtering
  * process such as reverberation several times. For cases where the iteration
  * index is needed, the full form as shown above can be used instead.
  *
  * ===Examples===
  *
  * {{{
  * // non-nested multi-channel signal reduced to mono (1)
  * play {
  *   Mix(SinOsc.ar(440 :: 660 :: Nil)) * 0.2 // --> SinOsc.ar(440) + SinOsc.ar(660)
  * }
  * }}}
  *
  * {{{
  * // non-nested multi-channel signal reduced to mono (2)
  * play {
  *   Mix(Pan2.ar(SinOsc.ar)) * 0.2 // --> left + right
  * }
  * }}}
  *
  * {{{
  * // mix inner channels
  * play {
  *   // --> [left(440) + left(660), right(440) + right(660)]
  *   Mix(Pan2.ar(SinOsc.ar(440 :: 660 :: Nil))) * 0.2
  * }
  * }}}
  *
  * {{{
  * // enforce monophonic mix
  * play {
  *   // --> left(440) + left(660) + right(440) + right(660)
  *   Mix.mono(Pan2.ar(SinOsc.ar(440 :: 660 :: Nil))) * 0.2
  * }
  * }}}
  *
  * {{{
  * // combine Mix(), Mix.fill(), Mix.fold()
  * // from original SC examples: reverberated sine percussion
  * play {
  *   val d = 6    // number of percolators
  *   val c = 5    // number of comb delays
  *   val a = 4    // number of allpass delays
  *
  *   // sine percolation sound :
  *   val s = Mix.fill(d) { Resonz.ar(Dust.ar(2.0 / d) * 50, Rand(200, 3200), 0.003) }
  *
  *   // reverb pre-delay time :
  *   val z = DelayN.ar(s, 0.048)
  *
  *   // 'c' length modulated comb delays in parallel :
  *   val y = Mix(CombL.ar(z, 0.1, LFNoise1.kr(Seq.fill(c)(Rand(0, 0.1))).madd(0.04, 0.05), 15))
  *
  *   // chain of 'a' allpass delays on each of two channels (2 times 'a' total) :
  *   val x = Mix.fold(y, a) { in =>
  *     AllpassN.ar(in, 0.050, Seq(Rand(0, 0.050), Rand(0, 0.050)), 1)
  *   }
  *
  *   // add original sound to reverb and play it :
  *   s + 0.2 * x
  * }
  * }}}
  *
  * {{{
  * // Mix.tabulate usage
  * // from original SC examples: harmonic swimming
  * play {
  *   val f = 50       // fundamental frequency
  *   val p = 20       // number of partials per channel
  *   val offset = Line.kr(0, -0.02, 60, doneAction = freeSelf) // causes sound to separate and fade
  *   Mix.tabulate(p) { i =>
  *     FSinOsc.ar(f * (i+1)) * // freq of partial
  *       LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10)))  // amplitude rate
  *       .madd(
  *         0.02,     // amplitude scale
  *         offset    // amplitude offset
  *       ).max(0)    // clip negative amplitudes to zero
  *   }
  * }
  * }}}
  *
  * @see [[de.sciss.synth.ugen.Reduce$ Reduce]]
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  */
object Mix extends ProductReader[Mix] {
  /** A mixing idiom that corresponds to `Seq.tabulate` and to `Array.fill` in sclang. */
  def tabulate(n: Int)(fun: Int => GE): GE = Mix(GESeq(Vector.tabulate(n)(i => fun(i))))

  /** A mixing idiom that corresponds to `Seq.fill`. */
  def fill(n: Int)(thunk: => GE): GE = Mix(GESeq(Vector.fill(n)(thunk)))

  /** A shortcut for `Mix(GESeq(elems: _*))`. */
  def seq(elems: GE*): GE = Mix(GESeq(elems.toIndexedSeq))

  /** A special mix that flattens all channels of
    * the input element before summing them, guaranteeing that result is monophonic.
    */
  def mono(elem: GE): GE = Mono(elem)

  /** A mixing idiom that is not actually adding elements, but recursively folding them.
    *
    * Calling this method is equivalent to
    * {{{
    * (1 to n).foldLeft(elem) { (res, _) => fun(res) }
    * }}}
    *
    * It is often used in the SuperCollider examples to apply a filtering process such as reverberation
    * several times. For cases where the iteration index is needed, the full form as shown above
    * can be used instead.
    *
    * @param elem the input element
    * @param n    the number of iterations
    * @param fun  a function that is recursively applied to produce the output
    */
  def fold(elem: GE, n: Int)(fun: GE => GE): GE = (1 to n).foldLeft(elem) { (res, _) => fun(res) }

  override def read(in: RefMapIn, prefix: String, arity: Int): Mix = {
    require (arity == 1)
    val _elem = in.readGE()
    new Mix(_elem)
  }

  object Mono extends ProductReader[Mono] {
    override def read(in: RefMapIn, prefix: String, arity: Int): Mono = {
      require (arity == 1)
      val _elem = in.readGE()
      new Mono(_elem)
    }
  }
  final case class Mono(elem: GE) extends GE.Lazy {
    def numOutputs  = 1
    def rate: MaybeRate = elem.rate
    override def productPrefix = s"Mix$$Mono"

    override def toString = s"$productPrefix($elem)"

    protected def makeUGens: UGenInLike = {
      val flat = elem.expand.flatOutputs
      Mix.makeUGen(flat)
    }
  }

  @tailrec private def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val sz = args.size
    if (sz == 0) UGenInGroup.empty else if (sz <= 4) make1(args) else {
      val mod = sz % 4
      if (mod == 0) {
        makeUGen(args.grouped(4).map(make1).toIndexedSeq)
      } else {
        // we keep the 1 to 3 tail elements, because
        // then the nested `makeUGen` call may again
        // call `make1` with the `Sum4` instances included.
        // E.g., if args.size is 6, we get `Sum3(Sum4(_,_,_,_),_,_)`,
        // whereas we would get `Plus(Plus(Sum4(_,_,_,_),_),_)`
        // if we just used the mod-0 branch.
        val (init, tail) = args.splitAt(sz - mod)
        val quad = init.grouped(4).map(make1).toIndexedSeq
        makeUGen(quad ++ tail)
      }
    }
  }

  private def make1(args: Vec[UGenIn]): UGenIn = {
    import BinaryOpUGen.Plus
    val sz = args.size
    (sz: @switch) match {
      case 1 => args.head
      case 2 => Plus.make1(args(0), args(1))
      case 3 => Sum3.make1(args)
      case 4 => Sum4.make1(args)
    }
  }
}

/** A graph element that mixes the channels of a signal together.
  * It works like the sclang counterpart.
  *
  * The `Mix` companion object contains various useful mixing idioms:
  *
  * - `Mix.tabulate(n: Int)(fun: Int => GE)`: corresponds to `Seq.tabulate`
  *   and to `Array.fill` in sclang.
  * - `Mix.fill(n: Int)(thunk: => GE)`: corresponds to `Seq.fill`.
  * - `Mix.seq(elems: GE*)`: A shortcut for `Mix(GESeq(elems: _*))`.
  *
  * A separate graph element is `Mix.mono`.
  * `Mix.mono(elem: GE)` flattens all channels of
  * the input element before summing them, guaranteeing that the result is monophonic.
  *
  * Finally, `Mix.fold` is an idiom that not actually adds elements,
  * but recursively folds them. Thus,
  * `Mix.fold(elem: GE, n: Int)(fun: GE => GE)` is equivalent to
  * {{{
  * (1 to n).foldLeft(elem) { (res, _) => fun(res) }
  * }}}
  *
  * `Mix.fold` is often used in the SuperCollider examples to apply a filtering
  * process such as reverberation several times. For cases where the iteration
  * index is needed, the full form as shown above can be used instead.
  *
  * @param  elem  the graph element whose channels to mix together
  *
  * @see [[de.sciss.synth.ugen.Reduce$ Reduce]]
  * @see [[de.sciss.synth.ugen.BinaryOpUGen$ BinaryOpUGen]]
  */
final case class Mix(elem: GE) extends UGenSource.SingleOut {  // XXX TODO: should not be UGenSource

  def rate: MaybeRate = elem.rate

  protected def makeUGens: UGenInLike = unwrap(this, elem.expand.outputs)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = Mix.makeUGen(args)

  override def toString: String = elem match {
    case GESeq(elems) => elems.mkString(s"$productPrefix.seq(", ", ", ")")
    case _            => s"$productPrefix($elem)"
  }
}

object Zip extends ProductReader[Zip] {
  override def read(in: RefMapIn, prefix: String, arity: Int): Zip = {
    require (arity == 1)
    val _elems = in.readGEVec()
    new Zip(_elems: _*)
  }
}
/** A graph element that interleaves a number of (multi-channel) input signals.
  * For example, if two stereo-signals `a` and `b` are zipped, the output will be a four-channel
  * signal corresponding to `[ a out 0, b out 0, a out 1, b out 1 ]`. If the input signals
  * have different numbers of channels, the minimum number of channels is used.
  *
  * ===Examples===
  *
  * {{{
  * // peak and RMS metering
  * val x = play {
  *   val sig   = PhysicalIn.ar(0 to 1)  // stereo input
  *   val tr    = Impulse.kr(5)
  *   val peak  = Peak.kr(sig, tr)
  *   val rms   = A2K.kr(Lag.ar(sig.squared, 0.1))
  *   SendReply.kr(tr, Zip(peak, rms), "/meter")
  * }
  *
  * val r = message.Responder.add(x.server) {
  *   case osc.Message("/meter", x.id, _, peakL: Float, rmsL: Float, peakR: Float, rmsR: Float) =>
  *     println(f"peak-left \$peakL%g, rms-left \$rmsL%g, peak-right \$peakR%g, rms-right \$rmsR%g")
  *
  * x.free(); r.remove()
  * }}}
  *
  * @param elems  the signals to interleave in a multi-channel output signal
  */
final case class Zip(elems: GE*) extends GE.Lazy {
  def rate: MaybeRate = MaybeRate.reduce(elems.map(_.rate): _*)

  protected def makeUGens: UGenInLike = {
    val exp   = elems.map(_.expand)
    val sz    = exp.map(_.outputs.size)
    val minSz = sz.min
    UGenInGroup((0 until minSz).flatMap(i => exp.map(_.unwrap(i))))
  }
}

object Reduce extends ProductReader[Reduce] {
  import BinaryOpUGen._
  /** Same result as `Mix( _ )` */
  def +  (elem: GE): Reduce = apply(elem, Plus  )
  def *  (elem: GE): Reduce = apply(elem, Times )
  //   def all_sig_==( elem: GE ) = ...
  //   def all_sig_!=( elem: GE ) = ...
  def min(elem: GE): Reduce = apply(elem, Min   )
  def max(elem: GE): Reduce = apply(elem, Max   )
  def &  (elem: GE): Reduce = apply(elem, BitAnd)
  def |  (elem: GE): Reduce = apply(elem, BitOr )
  def ^  (elem: GE): Reduce = apply(elem, BitXor)

  override def read(in: RefMapIn, prefix: String, arity: Int): Reduce = {
    require (arity == 2)
    val _elem = in.readGE()
    val _op   = in.readProductT[BinaryOpUGen.Op]()
    new Reduce(_elem, _op)
  }
}

final case class Reduce(elem: GE, op: BinaryOpUGen.Op) extends UGenSource.SingleOut {
  // XXX TODO: should not be UGenSource

  def rate: MaybeRate = elem.rate

  protected def makeUGens: UGenInLike = unwrap(this, elem.expand.outputs)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = args match {
    case head +: tail => tail.foldLeft(head)(op.make1)
    case _            => UGenInGroup.empty
  }
}

/** An element which writes an input signal to a bus, optionally applying a short fade-in.
  * This is automatically added when using the `play { ... }` syntax. If the fade time is
  * given, an envelope is added with a control named `"gate"` which can be used to release
  * the synth. The bus is given by a control named `"out"` and defaults to zero.
  */
object WrapOut extends ProductReader[WrapOut] {
  private def makeFadeEnv(fadeTime: Double): UGenIn = {
    val cFadeTime = "fadeTime".kr(fadeTime)
    val cGate     = "gate".kr(1f)
    val startVal  = cFadeTime <= 0f
    val env       = Env(startVal, List(Env.Segment(1, 1, Curve.parametric(-4)), Env.Segment(1, 0, Curve.sine)), 1)
    val res       = EnvGen.kr(env, gate = cGate, timeScale = cFadeTime, doneAction = freeSelf)
    res.expand.flatOutputs.head
  }

  override def read(in: RefMapIn, prefix: String, arity: Int): WrapOut = {
    require (arity == 2)
    val _in       = in.readGE()
    val _fadeTime = in.readDouble()
    new WrapOut(_in, _fadeTime)
  }
}

// XXX TODO: This should not be a UGenSource.ZeroOut but just a Lazy.Expander[Unit] !
/** An element which writes an input signal to a bus, optionally applying a short fade-in.
  * This is automatically added when using the `play { ... }` syntax. If the fade time is
  * given, an envelope is added with a control named `"gate"` which can be used to release
  * the synth. The bus is given by a control named `"out"` and defaults to zero.
  *
  * @param  in        the signal to play to the default output
  * @param  fadeTime  the fade in time; use a negative number for no fading
  */
final case class WrapOut(in: GE, fadeTime: Double = 0.02) extends UGenSource.ZeroOut with WritesBus {
  import WrapOut._

  protected def makeUGens: Unit = unwrap(this, in.expand.outputs)

  protected def makeUGen(ins: Vec[UGenIn]): Unit = {
    if (ins.isEmpty) return
    val rate = ins.map(_.rate).max
    if ((rate == audio) || (rate == control)) {
      val ins3 = if (fadeTime >= 0) {
        val env = makeFadeEnv(fadeTime)
        ins.map(BinaryOpUGen.Times.make1(_, env))
      } else {
        ins
      }
      val cOut = "out".kr(0f)
      Out.ar(cOut, ins3)
      ()
    }
  }
}

/** A graph element that spreads a sequence of input channels across a ring of output channels.
  * This works by feeding each input channel through a dedicated `PanAz` UGen, and mixing the
  * results together.
  *
  * The panning position of each input channel with index `ch` is calculated by the formula:
  * {{{
  * val pf = 2.0 / (num-in-channels - 1) * (num-out-channels - 1) / num-out-channels
  * ch * pf + center
  * }}}
  *
  * @see  [[de.sciss.synth.ugen.PanAz$ PanAz]]
  */
object SplayAz extends ProductReader[SplayAz] {
  /** @param numChannels  the number of output channels
    * @param in           the input signal
    * @param spread       the spacing between input channels with respect to the output panning
    * @param center       the position of the first channel (see `PanAz`)
    * @param level        a global gain factor (see `PanAz`)
    * @param width        the `width` parameter for each `PanAz`
    * @param orient       the `orient` parameter for each `PanAz`
    */
  def ar(numChannels: Int, in: GE, spread: GE = 1f, center: GE = 0f, level: GE = 1f,
         width: GE = 2f, orient: GE = 0f): SplayAz =
    apply(audio, numChannels, in, spread, center, level, width, orient)

  override def read(in: RefMapIn, prefix: String, arity: Int): SplayAz = {
    require (arity == 8)
    val _rate         = in.readRate()
    val _numChannels  = in.readInt()
    val _in           = in.readGE()
    val _spread       = in.readGE()
    val _center       = in.readGE()
    val _level        = in.readGE()
    val _width        = in.readGE()
    val _orient       = in.readGE()
    new SplayAz(_rate, _numChannels, _in, _spread, _center, _level, _width, _orient)
  }
}
/** A graph element that spreads a sequence of input channels across a ring of output channels.
  * This works by feeding each input channel through a dedicated `PanAz` UGen, and mixing the
  * results together.
  *
  * The panning position of each input channel with index `ch` is calculated by the formula:
  * {{{
  * val pf = 2.0 / (num-in-channels - 1) * (num-out-channels - 1) / num-out-channels
  * ch * pf + center
  * }}}
  *
  * @param numChannels  the number of output channels
  * @param in           the input signal
  * @param spread       the spacing between input channels with respect to the output panning
  * @param center       the position of the first channel (see `PanAz`)
  * @param level        a global gain factor (see `PanAz`)
  * @param width        the `width` parameter for each `PanAz`
  * @param orient       the `orient` parameter for each `PanAz`
  *
  * @see  [[de.sciss.synth.ugen.PanAz$ PanAz]]
  */
final case class SplayAz(rate: Rate, numChannels: Int, in: GE, spread: GE, center: GE, level: GE, width: GE, orient: GE)
  extends GE.Lazy {

  def numOutputs: Int = numChannels

  protected def makeUGens: UGenInLike = {
    val _in     = in.expand
    val numIn   = _in.outputs.size
    // last channel must have position (2 * i / (numIn - 1) * (numOut - 1) / numOut)
    val pf  = if (numIn < 2 || numOutputs == 0) 0.0 else 2.0 / (numIn - 1) * (numOutputs - 1) / numOutputs
    val pos = Seq.tabulate(numIn)(center + _ * pf)
    val mix = Mix(PanAz(rate, numChannels, _in, pos, level, width, orient))
    mix
  }
}

object LinLin extends ProductReader[LinLin] {
  override def read(in: RefMapIn, prefix: String, arity: Int): LinLin = {
    require (arity == 5)
    val _in     = in.readGE()
    val _srcLo  = in.readGE()
    val _srcHi  = in.readGE()
    val _dstLo  = in.readGE()
    val _dstHi  = in.readGE()
    new LinLin(_in, _srcLo, _srcHi, _dstLo, _dstHi)
  }
}
/** A graph element which maps a linear range to another linear range.
  * The equivalent formula is `(in - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo`.
  *
  * '''Note''': No clipping is performed. If the input signal exceeds the input range,
  * the output will also exceed its range.
  *
  * ===Examples===
  *
  * {{{
  * // oscillator to frequency range
  * play {
  *   val mod = SinOsc.kr(Line.kr(1, 10, 10))
  *   SinOsc.ar(LinLin(mod, -1, 1, 100, 900)) * 0.1
  * }
  * }}}
  *
  * @param in              The input signal to convert.
  * @param srcLo           The lower limit of input range.
  * @param srcHi           The upper limit of input range.
  * @param dstLo           The lower limit of output range.
  * @param dstHi           The upper limit of output range.
  *
  * @see [[de.sciss.synth.ugen.LinExp$ LinExp]]
  * @see [[de.sciss.synth.ugen.Clip$ Clip]]
  * @see [[de.sciss.synth.ugen.MulAdd MulAdd]]
  */
final case class LinLin(/* rate: MaybeRate, */ in: GE, srcLo: GE = 0f, srcHi: GE = 1f, dstLo: GE = 0f, dstHi: GE = 1f)
  extends GE.Lazy {

  def rate: MaybeRate = in.rate // XXX correct?

  protected def makeUGens: UGenInLike = {
    val scale  = (dstHi - dstLo) / (srcHi - srcLo)
    val offset = dstLo - (scale * srcLo)
    MulAdd(in, scale, offset)
  }
}

/** A graph element that produces a constant silent
  * (zero) audio-rate output signal.
  *
  * @see [[de.sciss.synth.ugen.DC$ DC]]
  */
object Silent extends ProductReader[Silent] {
  def ar: Silent = ar()

  def ar(numChannels: Int = 1): Silent = apply(numChannels)

  override def read(in: RefMapIn, prefix: String, arity: Int): Silent = {
    require (arity == 1)
    val _numChannels = in.readInt()
    new Silent(_numChannels)
  }
}
/** A graph element that produces a constant silent
  * (zero) audio-rate output signal.
  *
  * @param numChannels  the number of output channels
  *
  * @see [[de.sciss.synth.ugen.DC$ DC]]
  */
final case class Silent(numChannels: Int) extends GE.Lazy with AudioRated {

  protected def makeUGens: UGenInLike = {
    val dc = DC.ar(0)
    val ge: GE = if (numChannels == 1) dc else Seq.fill(numChannels)(dc)
    ge
  }
}

/** A graph element which reads from a connected sound driver input. This is a convenience
  * element for accessing physical input signals, e.g. from a microphone connected to your
  * audio interface. It expands to a regular `In` UGen offset by `NumOutputBuses.ir`.
  *
  * For example, consider an audio interface with channels 1 to 8 being analog line inputs,
  * channels 9 and 10 being AES/EBU and channels 11 to 18 being ADAT inputs. To read a combination
  * of the analog and ADAT inputs, either of the following statement can be used:
  *
  * {{{
  * PhysicalIn(Seq(0, 8), Seq(8, 8))
  * PhysicalIn(Seq(0, 8), Seq(8))      // numChannels wraps!
  * }}}
  *
  * If SuperCollider runs with less physical inputs than requested by this UGen,
  * invalid channels are muted.
  */
object PhysicalIn extends ProductReader[PhysicalIn] {
  /** Short cut for reading a mono signal from the first physical input. */
  def ar: PhysicalIn = ar()

  /** @param indices       the physical index to read from (beginning at zero which corresponds to
    *                      the first channel of the audio interface or sound driver). It may be a
    *                      multichannel element to specify discrete indices.
    * @param numChannels   the number of consecutive channels to read. For discrete indices this
    *                      applies to each index!
    */
  def ar(indices: GE = 0, numChannels: Int = 1): PhysicalIn = apply(indices, Seq(numChannels))

  /** @param indices       the physical index to read from (beginning at zero which corresponds to
    *                      the first channel of the audio interface or sound driver). It may be a
    *                      multichannel element to specify discrete indices.
    * @param numChannels   the number of consecutive channels to read for each index. Wraps around
    *                      if the sequence has less elements than indices has channels.
    */
  def ar(indices: GE, numChannels: Seq[Int]): PhysicalIn = apply(indices, numChannels)

  override def read(in: RefMapIn, prefix: String, arity: Int): PhysicalIn = {
    require (arity == 2)
    val _indices      = in.readGE()
    val _numChannels  = in.readIntVec()
    new PhysicalIn(_indices, _numChannels)
  }
}

/** A graph element which reads from a connected sound driver input. This is a convenience
  * element for accessing physical input signals, e.g. from a microphone connected to your
  * audio interface. It expands to a regular `In` UGen offset by `NumOutputBuses.ir`.
  *
  * For example, consider an audio interface with channels 1 to 8 being analog line inputs,
  * channels 9 and 10 being AES/EBU and channels 11 to 18 being ADAT inputs. To read a combination
  * of the analog and ADAT inputs, either of the following statement can be used:
  *
  * {{{
  * PhysicalIn(Seq(0, 8), Seq(8, 8))
  * PhysicalIn(Seq(0, 8), Seq(8))      // numChannels wraps!
  * }}}
  *
  * If SuperCollider runs with less physical inputs than requested by this UGen,
  * invalid channels are muted.
  *
  * @param indices       the physical index to read from (beginning at zero which corresponds to
  *                      the first channel of the audio interface or sound driver). It may be a
  *                      multichannel element to specify discrete indices.
  * @param numChannels   the number of consecutive channels to read for each index. Wraps around
  *                      if the sequence has less elements than indices has channels.
  */
final case class PhysicalIn(indices: GE, numChannels: Seq[Int]) extends GE.Lazy with AudioRated {

  protected def makeUGens: UGenInLike = {
    val numIn        = NumInputBuses .ir
    val numOut       = NumOutputBuses.ir

    val _indices     = indices.expand.outputs
    val iNumCh       = numChannels.toIndexedSeq
    val _numChannels = if (_indices.size <= iNumCh.size) iNumCh
    else {
      Vector.tabulate(_indices.size)(ch => iNumCh(ch % iNumCh.size))
    }

    val ins: GE = (_indices zip _numChannels).map {
      case (index, num) =>
        val in = In.ar(index + numOut, num)
        val ok = (0 until num).map(off => index + off < numIn)
        in * ok
    }

    // fix for #72
    val out = ins.expand.flatOutputs
    if (out.size == 1) out.head else out: GE
  }
}

/** A graph element which writes to a connected sound driver output. This is a convenience
  * element for `Out` with the ability to provide a set of discrete indices to which
  * corresponding channels of the input signal are mapped, whereas multichannel expansion
  * with respect to the index argument of `Out` typically do not achieve what you expect.
  *
  * If SuperCollider runs with less physical outputs than requested by this UGen,
  * the output is muted.
  *
  * ===Examples===
  *
  * {{{
  * // flip left and right when writing a stereo signal
  * play {
  *   val indices = Seq(1, 0)
  *   val in:GE   = Seq(SinOsc.ar * LFPulse.ar(4), WhiteNoise.ar)
  *   // sine appears on the right channel, and noise on the left
  *   PhysicalOut(indices, in * 0.2)
  * }
  * }}}
  */
object PhysicalOut extends ProductReader[PhysicalOut] {
  /** @param indices       the physical index to write to (beginning at zero which corresponds to
    *                      the first channel of the audio interface or sound driver). may be a
    *                      multichannel argument to specify discrete channels. In this case, any
    *                      remaining channels in `in` are associated with the last bus index offset.
    * @param in            the signal to write
    */
  def ar(indices: GE = 0, in: GE): PhysicalOut = apply(indices, in)

  override def read(in: RefMapIn, prefix: String, arity: Int): PhysicalOut = {
    require (arity == 2)
    val _indices  = in.readGE()
    val _in       = in.readGE()
    new PhysicalOut(_indices, _in)
  }
}

/** A graph element which writes to a connected sound driver output. This is a convenience
  * element for `Out` with the ability to provide a set of discrete indices to which
  * corresponding channels of the input signal are mapped, whereas multichannel expansion
  * with respect to the index argument of `Out` typically do not achieve what you expect.
  *
  * If SuperCollider runs with less physical outputs than requested by this UGen,
  * the output is muted.
  *
  * @param indices       the physical index to write to (beginning at zero which corresponds to
  *                      the first channel of the audio interface or sound driver). may be a
  *                      multichannel argument to specify discrete channels. In this case, any
  *                      remaining channels in `in` are associated with the last bus index offset.
  * @param in            the signal to write
  */
final case class PhysicalOut(indices: GE, in: GE) extends UGenSource.ZeroOut with AudioRated {
  // XXX TODO: should not be UGenSource

  protected def makeUGens: Unit = {
    val numOut    = NumOutputBuses.ir
    val _in       = in.expand.outputs
    val _indices  = indices.expand.outputs
    _indices.dropRight(1).zip(_in).foreach {
      case (index, sig) =>
        val ok = index + NumChannels(sig) <= numOut
        // Out.ar(index, sig)
        XOut.ar(index, in = sig, xfade = ok)
    }
    (_indices.lastOption, _in.drop(_indices.size - 1)) match {
      case (Some(index), sig) if sig.nonEmpty =>
        val ok = index + NumChannels(sig) <= numOut
        // Out.ar(index, sig)
        XOut.ar(index, in = sig, xfade = ok)
        ()
      case _ =>
    }
  }

  protected def makeUGen(args: Vec[UGenIn]): Unit = () // XXX not used, ugly
}

object RepeatChannels extends ProductReader[RepeatChannels] {
  override def read(in: RefMapIn, prefix: String, arity: Int): RepeatChannels = {
    require (arity == 2)
    val _a    = in.readGE()
    val _num  = in.readInt()
    new RepeatChannels(_a, _num)
  }
}
/** An auxiliary graph element that repeats
  * the channels of an input signal, allowing
  * for example for an exhaustive element-wise
  * combination with another signal.
  *
  * Normally, the way multi-channel expansion
  * works is that when two signals are combined,
  * the output signal has a number of channels
  * that is the ''maximum'' of the individual number
  * of channels, and channels will be automatically
  * wrapped around.
  *
  * For example, in `x * y` if `x` has three and
  * `y` has five channels, the result expands to
  *
  * {{{
  * Seq[GE](
  *   x.out(0) * y.out(0), x.out(1) * y.out(1),
  *   x.out(2) * y.out(2), x.out(0) * y.out(3),
  *   x.out(1) * y.out(4)
  * )
  * }}}
  *
  * Using this element, we can enforce the appearance
  * of all combinations of channels, resulting in a signal
  * whose number of channels is the ''sum'' of the individual
  * number of channels.
  *
  * For example, `RepeatChannels(x, 5)` expands to
  *
  * {{{
  * Seq[GE](
  *   x.out(0), x.out(0), x.out(0), x.out(0), x.out(0),
  *   x.out(1), x.out(1), x.out(1), x.out(1), x.out(1),
  *   x.out(2), x.out(2), x.out(2), x.out(2), x.out(2)
  * )
  * }}}
  *
  * And `RepeatChannels(x, 5) * y` accordingly expands to
  * the fifteen-channels signal
  *
  * {{{
  * Seq[GE](
  *   (x out 0) * (y out 0), (x out 0) * (y out 1), (x out 0) * (y out 2), (x out 0) * (y out 3), (x out 0) * (y out 4),
  *   (x out 1) * (y out 0), (x out 1) * (y out 1), (x out 1) * (y out 2), (x out 1) * (y out 3), (x out 1) * (y out 4),
  *   (x out 2) * (y out 0), (x out 2) * (y out 1), (x out 2) * (y out 2), (x out 2) * (y out 3), (x out 2) * (y out 4)
  * )
  * }}}
  *
  * @param  a   the signal whose channels to repeat
  * @param  num the number of repetitions for each input channel
  *
  * @see  [[ChannelRangeProxy]]
  */
final case class RepeatChannels(a: GE, num: Int) extends GE.Lazy {
  def rate: MaybeRate = a.rate

  protected def makeUGens: UGenInLike = {
    val out = a.expand.outputs
    val seq = Seq.fill(num)(out).transpose.flatten
    seq: GE
  }
}

object ChannelRangeProxy extends ProductReader[ChannelRangeProxy] {
  override def read(in: RefMapIn, prefix: String, arity: Int): ChannelRangeProxy = {
    require (arity == 4)
    val _elem   = in.readGE()
    val _from   = in.readInt()
    val _until  = in.readInt()
    val _step   = in.readInt()
    new ChannelRangeProxy(_elem, _from, _until, _step)
  }
}
/** A helper graph element that selects a particular range of
  * output channel of another element. The range is specified with
  * integers and thus cannot be determined at graph expansion time.
  * If this is desired, the `Select` UGen can be used.
  *
  * Usually the graph element operator `out` along with
  * a standard Scala `Range` argument can be used
  * instead of explicitly writing `ChannelRangeProxy`. Thus
  * `elem out (0 until 4)` selects the first four channels and is
  * equivalent to `ChannelRangeProxy(elem, from = 0, until = 4, step = 1)`.
  *
  * Behind the scene, `ChannelProxy` instances are created, thus
  * `ChannelRangeProxy(x, a, b)` is the same as
  * `(a until b).map(ChannelProxy(x, _)): GE`.
  *
  * Because ScalaCollider allows late-expanding
  * graph elements, we have no direct way to get some
  * array of a UGen's outputs.
  *
  * @param  elem  a multi-channel element from which to select channels.
  * @param  from  the first index (inclusive) of the channel range, counting from zero.
  * @param  until the end index (exclusive) of the channel range, counting from zero.
  * @param  step  the increment from index to index in the range. A value of one
  *               means all channels from `from` until `until` will be selected. A
  *               value of two means, every second channel will be skipped. A negative
  *               value can be used to count down from high to low indices.
  *
  * @see [[de.sciss.synth.ugen.NumChannels NumChannels]]
  * @see [[de.sciss.synth.ugen.Select$ Select]]
  * @see [[de.sciss.synth.ugen.ChannelProxy ChannelProxy]]
  * @see [[de.sciss.synth.ugen.RepeatChannels RepeatChannels]]
  */
final case class ChannelRangeProxy(elem: GE, from: Int, until: Int, step: Int) extends GE.Lazy {
  def rate: MaybeRate = elem.rate

  def range: Range = Range(from, until, step)

  override def toString: String =
    if (step == 1) s"$elem.out($from until $until)" else s"$elem.out($from until $until by $step)"

  def makeUGens: UGenInLike = {
    val r = range
    if (r.isEmpty) return UGenInGroup.empty

    //    val _elem = elem.expand
    //    UGenInGroup(r.map(_elem.unwrap))

    GESeq(range.map(index => ChannelProxy(elem, index).expand))
  }
}

object ChannelIndices extends ProductReader[ChannelIndices] {
  override def read(in: RefMapIn, prefix: String, arity: Int): ChannelIndices = {
    require (arity == 1)
    val _in = in.readGE()
    new ChannelIndices(_in)
  }
}
/** A graph element that produces an integer sequence
  * from zero until the number-of-channels of the input element.
  *
  * ===Examples===
  *
  * {{{
  * // cross-faded select
  * play {
  *   val sines: GE = Seq.fill(4)(SinOsc.ar(ExpRand(200, 2000)))
  *   val index   = MouseX.kr(lo = 0, hi = NumChannels(sines) - 1)
  *   val indices = ChannelIndices(sines)
  *   indices.poll(0, "indices")
  *   val select  = 1 - (indices absdif index).min(1)
  *   val sig     = Mix(sines * select)
  *   sig * 0.2
  * }
  * }}}
  *
  * @param in the element whose indices to produce
  *
  * @see [[de.sciss.synth.ugen.NumChannels NumChannels]]
  */
final case class ChannelIndices(in: GE) extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = unwrap(this, in.expand.outputs)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = args.indices: GE
}

object NumChannels extends ProductReader[NumChannels] {
  override def read(in: RefMapIn, prefix: String, arity: Int): NumChannels = {
    require (arity == 1)
    val _in = in.readGE()
    new NumChannels(_in)
  }
}
/** A graph element that produces an integer with number-of-channels of the input element.
  *
  * Because ScalaCollider allows late-expanding
  * graph elements, we have no direct way to get an integer of some
  * array-size of a UGen's outputs. On the other hand, there may be
  * sound synthesis definitions that can abstract over the number of
  * channels at definition time.
  *
  * ===Examples===
  *
  * {{{
  * // amplitude compensation
  * play {
  *   val sines: GE = Seq.fill(8)(SinOsc.ar(ExpRand(200, 2000)))
  *   val norm = Mix(sines) / NumChannels(sines)   // guarantee that they don't clip
  *   norm * 0.2
  * }
  * }}}
  *
  * @param in the element whose number-of-channels to produce
  *
  * @see [[de.sciss.synth.ugen.ChannelIndices ChannelIndices]]
  */
final case class NumChannels(in: GE) extends UGenSource.SingleOut with ScalarRated {
  protected def makeUGens: UGenInLike = unwrap(this, in.expand.outputs)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = Constant(args.size.toFloat)
}

/** A graph element that controls the multi-channel expansion of
  * its `in` argument to match the `to` argument by padding (extending
  * and wrapping) it.
  */
object Pad extends ProductReader[Pad] {
  /** Enforces multi-channel expansion for the input argument
    * even if it is passed into a vararg input of another UGen.
    * This is done by wrapping it in a `GESeq`.
    */
  def Split(in: GE): GE = GESeq(Vector(in))

  override def read(in: RefMapIn, prefix: String, arity: Int): Pad = {
    require (arity == 2)
    val _in = in.readGE()
    val _to = in.readGE()
    new Pad(_in, _to)
  }
}
/** A graph element that controls the multi-channel expansion of
  * its `in` argument to match the `to` argument by padding (extending
  * and wrapping) it.
  *
  * @param in the element to replicate
  * @param to the reference element that controls the multi-channel expansion.
  *           the signal itself is not used or output by `Pad`.
  */
final case class Pad(in: GE, to: GE) extends UGenSource.SingleOut {
  def rate: MaybeRate = in.rate

  protected def makeUGens: UGenInLike = unwrap(this, Vector(in.expand, to.expand))

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = args.head
}