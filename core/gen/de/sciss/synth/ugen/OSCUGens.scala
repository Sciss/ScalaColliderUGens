// revision: 9
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that uses an input signal as an index into an octave repeating table of
  * pitch classes. The input is truncated to an integer, and indices wrap around the
  * table and shift octaves as they do.
  * 
  * ===Examples===
  * 
  * {{{
  * // modal space where mouse x controls pitch step
  * play {
  *   // initialize the scale buffer (Dorian)
  *   val scale = Vector(0, 2, 3.2, 5, 7, 9, 10)
  *   val buf   = LocalBuf(scale.size)
  *   SetBuf(buf, scale)
  *   
  *   // base MIDI pitch
  *   val base  = DegreeToKey.kr(buf, in = MouseX.kr(0, 15), octave = 12) + 72
  *   val noise = LFNoise1.kr(Seq(3, 3)) * 0.04  // low freq stereo detuning
  *   // lead tone
  *   val lead  = SinOsc.ar((base + noise).midiCps)
  *   // drone 5ths
  *   val drone = RLPF.ar(LFPulse.ar(Seq(48.midiCps, 55.midiCps), 0.15),
  *                       SinOsc.kr(0.1).mulAdd(10, 72).midiCps, 0.1)
  *   val mix = (lead + drone) * 0.1
  *   // add some 70's euro-space-rock echo
  *   CombN.ar(mix, 0.31, 0.31, 2) + mix
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  */
object DegreeToKey {
  /** @param buf              buffer which contains the steps for each scale degree.
    * @param in               input index signal
    * @param octave           number of steps per octave in the scale.
    */
  def kr(buf: GE, in: GE, octave: GE = 12): DegreeToKey = new DegreeToKey(control, buf, in, octave)
  
  /** @param buf              buffer which contains the steps for each scale degree.
    * @param in               input index signal
    * @param octave           number of steps per octave in the scale.
    */
  def ar(buf: GE, in: GE, octave: GE = 12): DegreeToKey = new DegreeToKey(audio, buf, in, octave)
}

/** A UGen that uses an input signal as an index into an octave repeating table of
  * pitch classes. The input is truncated to an integer, and indices wrap around the
  * table and shift octaves as they do.
  * 
  * @param buf              buffer which contains the steps for each scale degree.
  * @param in               input index signal
  * @param octave           number of steps per octave in the scale.
  * 
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  */
final case class DegreeToKey(rate: Rate, buf: GE, in: GE, octave: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand, octave.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen which selects among a sequence of inputs, according to an index signal.
  * Note that, although only one signal of the `multi` input is let through at a
  * time, still all ugens are continuously running.
  * 
  * @see [[de.sciss.synth.ugen.TWindex$ TWindex]]
  */
object Select {
  /** @param index            an index signal into the channels of the `in` argument.
    *                         The index is automatically clipped to lie between `0`
    *                         and `in.numOutputs - 1` . The index is truncated to its
    *                         integer part (not rounded), hence using for instance an
    *                         index of `0.9` will still be interpreted as index `0` .
    * @param in               a graph element which is composed of the channels to be
    *                         indexed.
    */
  def ir(index: GE, in: GE): Select = new Select(scalar, index, in)
  
  /** @param index            an index signal into the channels of the `in` argument.
    *                         The index is automatically clipped to lie between `0`
    *                         and `in.numOutputs - 1` . The index is truncated to its
    *                         integer part (not rounded), hence using for instance an
    *                         index of `0.9` will still be interpreted as index `0` .
    * @param in               a graph element which is composed of the channels to be
    *                         indexed.
    */
  def kr(index: GE, in: GE): Select = new Select(control, index, in)
  
  /** @param index            an index signal into the channels of the `in` argument.
    *                         The index is automatically clipped to lie between `0`
    *                         and `in.numOutputs - 1` . The index is truncated to its
    *                         integer part (not rounded), hence using for instance an
    *                         index of `0.9` will still be interpreted as index `0` .
    * @param in               a graph element which is composed of the channels to be
    *                         indexed.
    */
  def ar(index: GE, in: GE): Select = new Select(audio, index, in)
}

/** A UGen which selects among a sequence of inputs, according to an index signal.
  * Note that, although only one signal of the `multi` input is let through at a
  * time, still all ugens are continuously running.
  * 
  * @param index            an index signal into the channels of the `in` argument.
  *                         The index is automatically clipped to lie between `0`
  *                         and `in.numOutputs - 1` . The index is truncated to its
  *                         integer part (not rounded), hence using for instance an
  *                         index of `0.9` will still be interpreted as index `0` .
  * @param in               a graph element which is composed of the channels to be
  *                         indexed.
  * 
  * @see [[de.sciss.synth.ugen.TWindex$ TWindex]]
  */
final case class Select(rate: Rate, index: GE, in: GE) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](index.expand).++(in.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRateFrom(_args, 1, audio) else _args
    UGen.SingleOut(name, rate, _args1)
  }
}

/** A UGen providing a probability-weighted index into a sequence upon receiving a
  * trigger.
  * 
  * When triggered, returns a random index value based the values of the channels
  * of the `prob` argument functioning as probabilities. The index is zero based,
  * hence goes from `0` to `prob.numOutputs - 1` .
  * 
  * By default the sequence of probabilities should sum to 1.0, however for
  * convenience, this can be achieved by the ugen when the `normalize` flag is set
  * to 1 (less efficient).
  * 
  * @see [[de.sciss.synth.ugen.Select$ Select]]
  */
object TWindex {
  /** @param trig             the trigger used to calculate a new index. a trigger
    *                         occurs when passing from non-positive to positive
    * @param prob             a multi-channel graph element, where the output
    *                         channels correspond to to the probabilities of their
    *                         respective indices being chosen.
    * @param normalize        `0` if the seq argument already sums up to 1.0 and thus
    *                         doesn't need normalization, `1` if the sum is not
    *                         guaranteed to be 1.0 and thus the ugen is asked to
    *                         provide the normalization.
    */
  def kr(trig: GE, prob: GE, normalize: GE = 0): TWindex = new TWindex(control, trig, prob, normalize)
  
  /** @param trig             the trigger used to calculate a new index. a trigger
    *                         occurs when passing from non-positive to positive
    * @param prob             a multi-channel graph element, where the output
    *                         channels correspond to to the probabilities of their
    *                         respective indices being chosen.
    * @param normalize        `0` if the seq argument already sums up to 1.0 and thus
    *                         doesn't need normalization, `1` if the sum is not
    *                         guaranteed to be 1.0 and thus the ugen is asked to
    *                         provide the normalization.
    */
  def ar(trig: GE, prob: GE, normalize: GE = 0): TWindex = new TWindex(audio, trig, prob, normalize)
}

/** A UGen providing a probability-weighted index into a sequence upon receiving a
  * trigger.
  * 
  * When triggered, returns a random index value based the values of the channels
  * of the `prob` argument functioning as probabilities. The index is zero based,
  * hence goes from `0` to `prob.numOutputs - 1` .
  * 
  * By default the sequence of probabilities should sum to 1.0, however for
  * convenience, this can be achieved by the ugen when the `normalize` flag is set
  * to 1 (less efficient).
  * 
  * @param trig             the trigger used to calculate a new index. a trigger
  *                         occurs when passing from non-positive to positive
  * @param prob             a multi-channel graph element, where the output
  *                         channels correspond to to the probabilities of their
  *                         respective indices being chosen.
  * @param normalize        `0` if the seq argument already sums up to 1.0 and thus
  *                         doesn't need normalization, `1` if the sum is not
  *                         guaranteed to be 1.0 and thus the ugen is asked to
  *                         provide the normalization.
  * 
  * @see [[de.sciss.synth.ugen.Select$ Select]]
  */
final case class TWindex(rate: Rate, trig: GE, prob: GE, normalize: GE = 0) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](trig.expand, prob.expand, normalize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A UGen which reads a single sample value from a buffer at a given index.
  * 
  * It uses the `in` argument as index into the buffer, truncating that argument to
  * an integer. Out-of-range index values are clipped to the valid range.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. For example, if the buffer has two frames and
  * two channels, index 0 corresponds to frame 0, channel 0, index 1 correspond to
  * frame 0, channel 1, index 2 corresponds to frame 1, channel 0, and index 3
  * corresponds to frame 1, channel 1.
  * 
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  * @see [[de.sciss.synth.ugen.IndexInBetween$ IndexInBetween]]
  * @see [[de.sciss.synth.ugen.DetectIndex$ DetectIndex]]
  */
object Index {
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def ir(buf: GE, in: GE = 0): Index = new Index(scalar, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def kr(buf: GE, in: GE = 0): Index = new Index(control, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def ar(buf: GE, in: GE = 0): Index = new Index(audio, buf, in)
}

/** A UGen which reads a single sample value from a buffer at a given index.
  * 
  * It uses the `in` argument as index into the buffer, truncating that argument to
  * an integer. Out-of-range index values are clipped to the valid range.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. For example, if the buffer has two frames and
  * two channels, index 0 corresponds to frame 0, channel 0, index 1 correspond to
  * frame 0, channel 1, index 2 corresponds to frame 1, channel 0, and index 3
  * corresponds to frame 1, channel 1.
  * 
  * @param buf              The buffer to read from.
  * @param in               The sample index into the buffer. This is truncated to
  *                         an integer automatically.
  * 
  * @see [[de.sciss.synth.ugen.BufRd$ BufRd]]
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  * @see [[de.sciss.synth.ugen.IndexInBetween$ IndexInBetween]]
  * @see [[de.sciss.synth.ugen.DetectIndex$ DetectIndex]]
  */
final case class Index(rate: Rate, buf: GE, in: GE = 0) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen which reads from a buffer at a given index, linearly interpolating
  * between neighboring points.
  * 
  * It uses the `in` argument as index into the buffer. Out-of-range index values
  * are clipped to the valid range. If the index has a fractional part, it is used
  * to interpolate between the buffer index at the floor and the buffer index at the
  * ceiling of the index argument.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. See the `Index` UGen for details.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.IndexInBetween$ IndexInBetween]]
  */
object IndexL {
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This can have a
    *                         fractional part.
    */
  def ir(buf: GE, in: GE = 0): IndexL = new IndexL(scalar, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This can have a
    *                         fractional part.
    */
  def kr(buf: GE, in: GE = 0): IndexL = new IndexL(control, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This can have a
    *                         fractional part.
    */
  def ar(buf: GE, in: GE = 0): IndexL = new IndexL(audio, buf, in)
}

/** A UGen which reads from a buffer at a given index, linearly interpolating
  * between neighboring points.
  * 
  * It uses the `in` argument as index into the buffer. Out-of-range index values
  * are clipped to the valid range. If the index has a fractional part, it is used
  * to interpolate between the buffer index at the floor and the buffer index at the
  * ceiling of the index argument.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. See the `Index` UGen for details.
  * 
  * @param buf              The buffer to read from.
  * @param in               The sample index into the buffer. This can have a
  *                         fractional part.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.IndexInBetween$ IndexInBetween]]
  */
final case class IndexL(rate: Rate, buf: GE, in: GE = 0) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen which reads a single sample value from a buffer at a given index.
  * 
  * It uses the `in` argument as index into the buffer, truncating that argument to
  * an integer. Out-of-range index values are "folded" inside the valid range.
  * Folding means reflecting the excess at the valid range's boundaries.
  * 
  * For example, if the buffer has four samples, index 4 is wrapped to index 2 (the
  * excess beyond the maximum index of 3 is 4 - 3 = 1, and the excess is folded so
  * that and 3 - 1 = 2), index 5 is folded to index 1, index -1 is folded to index
  * 1, index -2 is folded to index 2, etc.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. See the `Index` UGen for details.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  */
object FoldIndex {
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def ir(buf: GE, in: GE = 0): FoldIndex = new FoldIndex(scalar, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def kr(buf: GE, in: GE = 0): FoldIndex = new FoldIndex(control, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def ar(buf: GE, in: GE = 0): FoldIndex = new FoldIndex(audio, buf, in)
}

/** A UGen which reads a single sample value from a buffer at a given index.
  * 
  * It uses the `in` argument as index into the buffer, truncating that argument to
  * an integer. Out-of-range index values are "folded" inside the valid range.
  * Folding means reflecting the excess at the valid range's boundaries.
  * 
  * For example, if the buffer has four samples, index 4 is wrapped to index 2 (the
  * excess beyond the maximum index of 3 is 4 - 3 = 1, and the excess is folded so
  * that and 3 - 1 = 2), index 5 is folded to index 1, index -1 is folded to index
  * 1, index -2 is folded to index 2, etc.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. See the `Index` UGen for details.
  * 
  * @param buf              The buffer to read from.
  * @param in               The sample index into the buffer. This is truncated to
  *                         an integer automatically.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  */
final case class FoldIndex(rate: Rate, buf: GE, in: GE = 0) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen which reads a single sample value from a buffer at a given index.
  * 
  * It uses the `in` argument as index into the buffer, truncating that argument to
  * an integer. Out-of-range index values are wrapped around the valid range. For
  * example, if the buffer has four samples, index 4 is wrapped to index 0, index 5
  * is wrapped to index 1, index -1 is wrapped to index 3, index -2 is wrapped to
  * index 2, etc.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. See the `Index` UGen for details.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.FoldIndex$ FoldIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  */
object WrapIndex {
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def ir(buf: GE, in: GE = 0): WrapIndex = new WrapIndex(scalar, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def kr(buf: GE, in: GE = 0): WrapIndex = new WrapIndex(control, buf, in)
  
  /** @param buf              The buffer to read from.
    * @param in               The sample index into the buffer. This is truncated to
    *                         an integer automatically.
    */
  def ar(buf: GE, in: GE = 0): WrapIndex = new WrapIndex(audio, buf, in)
}

/** A UGen which reads a single sample value from a buffer at a given index.
  * 
  * It uses the `in` argument as index into the buffer, truncating that argument to
  * an integer. Out-of-range index values are wrapped around the valid range. For
  * example, if the buffer has four samples, index 4 is wrapped to index 0, index 5
  * is wrapped to index 1, index -1 is wrapped to index 3, index -2 is wrapped to
  * index 2, etc.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. See the `Index` UGen for details.
  * 
  * @param buf              The buffer to read from.
  * @param in               The sample index into the buffer. This is truncated to
  *                         an integer automatically.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.FoldIndex$ FoldIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  */
final case class WrapIndex(rate: Rate, buf: GE, in: GE = 0) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen which determines the (lowest) index in a buffer at which the two
  * neighboring values contain a given input signal. The output index is a decimal
  * whose fractional part is suitable for linearly interpolating between the buffer
  * slot values.
  * 
  * For example, if the Buffer contains values 3, 21, 25, 26 and the input signal
  * has the value 22, then the output will be 1.25, because the value 22 is
  * in-between the values stored at indices 1 and 2 and the linear location of 22 is
  * one-quarter of the way along the interval between them: 21 * (1 - 0.25) + 25 *
  * (1 - 0.75) = 22.
  * 
  * If the input value is smaller than the first sample, the output will be zero.
  * If the input value is larger than any sample in the buffer, the output will be
  * the buffer size minus one.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. For example, if the buffer has two frames and
  * two channels, and the algorithm finds the frame 1 in channel 0, the reported
  * index is 2 (frame * numChannels + channel).
  * 
  * `IndexInBetween` is the complement of the `IndexL` UGen.
  * 
  * @see [[de.sciss.synth.ugen.DetectIndex$ DetectIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  */
object IndexInBetween {
  /** @param buf              The buffer to search in.
    * @param in               The input signal whose value is looked up in the buffer.
    */
  def ir(buf: GE, in: GE): IndexInBetween = new IndexInBetween(scalar, buf, in)
  
  /** @param buf              The buffer to search in.
    * @param in               The input signal whose value is looked up in the buffer.
    */
  def kr(buf: GE, in: GE): IndexInBetween = new IndexInBetween(control, buf, in)
  
  /** @param buf              The buffer to search in.
    * @param in               The input signal whose value is looked up in the buffer.
    */
  def ar(buf: GE, in: GE): IndexInBetween = new IndexInBetween(audio, buf, in)
}

/** A UGen which determines the (lowest) index in a buffer at which the two
  * neighboring values contain a given input signal. The output index is a decimal
  * whose fractional part is suitable for linearly interpolating between the buffer
  * slot values.
  * 
  * For example, if the Buffer contains values 3, 21, 25, 26 and the input signal
  * has the value 22, then the output will be 1.25, because the value 22 is
  * in-between the values stored at indices 1 and 2 and the linear location of 22 is
  * one-quarter of the way along the interval between them: 21 * (1 - 0.25) + 25 *
  * (1 - 0.75) = 22.
  * 
  * If the input value is smaller than the first sample, the output will be zero.
  * If the input value is larger than any sample in the buffer, the output will be
  * the buffer size minus one.
  * 
  * While designed for monophonic buffers, it works with multi-channel buffers by
  * treating them as de-interleaved. For example, if the buffer has two frames and
  * two channels, and the algorithm finds the frame 1 in channel 0, the reported
  * index is 2 (frame * numChannels + channel).
  * 
  * `IndexInBetween` is the complement of the `IndexL` UGen.
  * 
  * @param buf              The buffer to search in.
  * @param in               The input signal whose value is looked up in the buffer.
  * 
  * @see [[de.sciss.synth.ugen.DetectIndex$ DetectIndex]]
  * @see [[de.sciss.synth.ugen.IndexL$ IndexL]]
  */
final case class IndexInBetween(rate: Rate, buf: GE, in: GE) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen which determines the index in a buffer at which the value matches a
  * given input signal. If the input value is not found, it outputs -1.
  * 
  * For example, if the buffer contains values 5, 3, 2, 8, and the input signal is
  * 3, the output will be 1. If the input is 3.001, the output will be -1. Unlike
  * `IndexInBetween` , this UGen always searches through the entire buffer until the
  * value is found or the end has been reached (returning -1).
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.IndexInBetween$ IndexInBetween]]
  */
object DetectIndex {
  /** 
    */
  def ir(buf: GE, in: GE): DetectIndex = new DetectIndex(scalar, buf, in)
  
  /** 
    */
  def kr(buf: GE, in: GE): DetectIndex = new DetectIndex(control, buf, in)
  
  /** 
    */
  def ar(buf: GE, in: GE): DetectIndex = new DetectIndex(audio, buf, in)
}

/** A UGen which determines the index in a buffer at which the value matches a
  * given input signal. If the input value is not found, it outputs -1.
  * 
  * For example, if the buffer contains values 5, 3, 2, 8, and the input signal is
  * 3, the output will be 1. If the input is 3.001, the output will be -1. Unlike
  * `IndexInBetween` , this UGen always searches through the entire buffer until the
  * value is found or the end has been reached (returning -1).
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.IndexInBetween$ IndexInBetween]]
  */
final case class DetectIndex(rate: Rate, buf: GE, in: GE) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A waveshaping UGen. Waveshaping is a the process of translating an input signal
  * by indexing a table (buffer).
  * 
  * '''Advanced notes:''' wavetable format:
  * {{{
  * Signal: [a0, a1, a2...]
  * Wavetable: [2*a0-a1, a1-a0, 2*a1-a2, a2-a1, 2*a2-a3, a3-a2...]
  * }}}
  * This strange format is not a standard linear interpolation (integer + frac),
  * but for (integer part -1) and (1+frac)) due to some efficient maths for integer
  * to float conversion in the underlying C code.
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  */
object Shaper {
  /** @param buf              buffer filled in wavetable format containing the
    *                         transfer function.
    * @param in               signal to be fed into the wave shaper
    */
  def kr(buf: GE, in: GE): Shaper = new Shaper(control, buf, in)
  
  /** @param buf              buffer filled in wavetable format containing the
    *                         transfer function.
    * @param in               signal to be fed into the wave shaper
    */
  def ar(buf: GE, in: GE): Shaper = new Shaper(audio, buf, in)
}

/** A waveshaping UGen. Waveshaping is a the process of translating an input signal
  * by indexing a table (buffer).
  * 
  * '''Advanced notes:''' wavetable format:
  * {{{
  * Signal: [a0, a1, a2...]
  * Wavetable: [2*a0-a1, a1-a0, 2*a1-a2, a2-a1, 2*a2-a3, a3-a2...]
  * }}}
  * This strange format is not a standard linear interpolation (integer + frac),
  * but for (integer part -1) and (1+frac)) due to some efficient maths for integer
  * to float conversion in the underlying C code.
  * 
  * @param buf              buffer filled in wavetable format containing the
  *                         transfer function.
  * @param in               signal to be fed into the wave shaper
  * 
  * @see [[de.sciss.synth.ugen.Index$ Index]]
  * @see [[de.sciss.synth.ugen.WrapIndex$ WrapIndex]]
  */
final case class Shaper(rate: Rate, buf: GE, in: GE) extends UGenSource.SingleOut with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, in.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A sine oscillator UGen using a fast approximation. It uses a ringing filter and
  * is less CPU expensive than `SinOsc` . However, the amplitude of the wave will
  * vary with frequency. Generally the amplitude will go down when the frequency
  * rises and it will go up as if the frequency is lowered.
  * 
  * '''Warning''': In the current implementation, the amplitude can blow up if the
  * frequency is modulated by certain alternating signals (e.g. abruptly by `TRand`
  * ).
  * 
  * ===Examples===
  * 
  * {{{
  * // plain oscillator
  * play { FSinOsc.ar(441) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SinOsc$ SinOsc]]
  * @see [[de.sciss.synth.ugen.SinOscFB$ SinOscFB]]
  */
object FSinOsc {
  def kr: FSinOsc = kr()
  
  /** @param freq             frequency in Hertz
    * @param iphase           initial phase of the oscillator in radians. This cannot
    *                         be modulated. A value of `0.5 Pi` means the output
    *                         starts at +1. A value of `1.5 Pi` means the output
    *                         starts at -1, ''(init-time only)''
    */
  def kr(freq: GE = 440.0f, iphase: GE = 0.0f): FSinOsc = new FSinOsc(control, freq, iphase)
  
  def ar: FSinOsc = ar()
  
  /** @param freq             frequency in Hertz
    * @param iphase           initial phase of the oscillator in radians. This cannot
    *                         be modulated. A value of `0.5 Pi` means the output
    *                         starts at +1. A value of `1.5 Pi` means the output
    *                         starts at -1, ''(init-time only)''
    */
  def ar(freq: GE = 440.0f, iphase: GE = 0.0f): FSinOsc = new FSinOsc(audio, freq, iphase)
}

/** A sine oscillator UGen using a fast approximation. It uses a ringing filter and
  * is less CPU expensive than `SinOsc` . However, the amplitude of the wave will
  * vary with frequency. Generally the amplitude will go down when the frequency
  * rises and it will go up as if the frequency is lowered.
  * 
  * '''Warning''': In the current implementation, the amplitude can blow up if the
  * frequency is modulated by certain alternating signals (e.g. abruptly by `TRand`
  * ).
  * 
  * @param freq             frequency in Hertz
  * @param iphase           initial phase of the oscillator in radians. This cannot
  *                         be modulated. A value of `0.5 Pi` means the output
  *                         starts at +1. A value of `1.5 Pi` means the output
  *                         starts at -1, ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.SinOsc$ SinOsc]]
  * @see [[de.sciss.synth.ugen.SinOscFB$ SinOscFB]]
  */
final case class FSinOsc(rate: Rate, freq: GE = 440.0f, iphase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, iphase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sinusoidal (sine tone) oscillator UGen. This is the same as `Osc` except that
  * it uses a built-in interpolating sine table of 8192 entries.
  * 
  * '''Note''' that currently (SC 3.7.x), the first frame generated is not zero
  * (i.e. the value of the sine oscillation at time zero) but the value at time
  * `1 / SampleRate.ir` .
  * 
  * ===Examples===
  * 
  * {{{
  * // plain oscillator
  * play { SinOsc.ar(441) * 0.2 }
  * }}}
  * {{{
  * // modulate frequency
  * play { SinOsc.ar(SinOsc.ar(XLine.kr(1, 1000, 9)).mulAdd(200, 800)) * 0.25 }
  * }}}
  * {{{
  * // modulate phase
  * play { SinOsc.ar(800, SinOsc.ar(XLine.kr(1, 1000, 9)) * 2*math.Pi) * 0.25 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Osc$ Osc]]
  * @see [[de.sciss.synth.ugen.FSinOsc$ FSinOsc]]
  * @see [[de.sciss.synth.ugen.SinOscFB$ SinOscFB]]
  */
object SinOsc {
  def kr: SinOsc = kr()
  
  /** @param freq             frequency in Hertz
    * @param phase            phase offset or modulator in radians
    */
  def kr(freq: GE = 440.0f, phase: GE = 0.0f): SinOsc = new SinOsc(control, freq, phase)
  
  def ar: SinOsc = ar()
  
  /** @param freq             frequency in Hertz
    * @param phase            phase offset or modulator in radians
    */
  def ar(freq: GE = 440.0f, phase: GE = 0.0f): SinOsc = new SinOsc(audio, freq, phase)
}

/** A sinusoidal (sine tone) oscillator UGen. This is the same as `Osc` except that
  * it uses a built-in interpolating sine table of 8192 entries.
  * 
  * '''Note''' that currently (SC 3.7.x), the first frame generated is not zero
  * (i.e. the value of the sine oscillation at time zero) but the value at time
  * `1 / SampleRate.ir` .
  * 
  * @param freq             frequency in Hertz
  * @param phase            phase offset or modulator in radians
  * 
  * @see [[de.sciss.synth.ugen.Osc$ Osc]]
  * @see [[de.sciss.synth.ugen.FSinOsc$ FSinOsc]]
  * @see [[de.sciss.synth.ugen.SinOscFB$ SinOscFB]]
  */
final case class SinOsc(rate: Rate, freq: GE = 440.0f, phase: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, phase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A sine oscillator UGen that has phase modulation feedback. Its output plugs
  * back into the phase input, allowing a modulation between a sine wave and a
  * sawtooth-like wave. "Over-modulation" causes chaotic oscillation. It may be
  * useful to simulate feedback FM synths.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse-controlled feedback
  * play { SinOscFB.ar(441, MouseX.kr(0, math.Pi)) * 0.1 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.SinOsc$ SinOsc]]
  * @see [[de.sciss.synth.ugen.FSinOsc$ FSinOsc]]
  */
object SinOscFB {
  def kr: SinOscFB = kr()
  
  /** @param freq             frequency in Hertz
    * @param feedback         the amplitude of phase feedback in radians. a value of
    *                         zero produces a clean sine wave.
    */
  def kr(freq: GE = 440.0f, feedback: GE = 0.0f): SinOscFB = new SinOscFB(control, freq, feedback)
  
  def ar: SinOscFB = ar()
  
  /** @param freq             frequency in Hertz
    * @param feedback         the amplitude of phase feedback in radians. a value of
    *                         zero produces a clean sine wave.
    */
  def ar(freq: GE = 440.0f, feedback: GE = 0.0f): SinOscFB = new SinOscFB(audio, freq, feedback)
}

/** A sine oscillator UGen that has phase modulation feedback. Its output plugs
  * back into the phase input, allowing a modulation between a sine wave and a
  * sawtooth-like wave. "Over-modulation" causes chaotic oscillation. It may be
  * useful to simulate feedback FM synths.
  * 
  * @param freq             frequency in Hertz
  * @param feedback         the amplitude of phase feedback in radians. a value of
  *                         zero produces a clean sine wave.
  * 
  * @see [[de.sciss.synth.ugen.SinOsc$ SinOsc]]
  * @see [[de.sciss.synth.ugen.FSinOsc$ FSinOsc]]
  */
final case class SinOscFB(rate: Rate, freq: GE = 440.0f, feedback: GE = 0.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, feedback.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}
object VOsc {
  def kr(bufPos: GE, freq: GE = 440.0f, phase: GE = 0.0f): VOsc = new VOsc(control, bufPos, freq, phase)
  
  def ar(bufPos: GE, freq: GE = 440.0f, phase: GE = 0.0f): VOsc = new VOsc(audio, bufPos, freq, phase)
}
final case class VOsc(rate: Rate, bufPos: GE, freq: GE = 440.0f, phase: GE = 0.0f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](bufPos.expand, freq.expand, phase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object VOsc3 {
  def kr(bufPos: GE, freq1: GE = 110.0f, freq2: GE = 220.0f, freq3: GE = 440.0f): VOsc3 = 
    new VOsc3(control, bufPos, freq1, freq2, freq3)
  
  def ar(bufPos: GE, freq1: GE = 110.0f, freq2: GE = 220.0f, freq3: GE = 440.0f): VOsc3 = 
    new VOsc3(audio, bufPos, freq1, freq2, freq3)
}
final case class VOsc3(rate: Rate, bufPos: GE, freq1: GE = 110.0f, freq2: GE = 220.0f, freq3: GE = 440.0f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](bufPos.expand, freq1.expand, freq2.expand, freq3.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** An oscillator UGen that linearly interpolates a wavetable. It has frequency and
  * phase modulation inputs. The wave table is provided by a buffer filled with a
  * wavetable format signal. The buffer size must be a power of 2.
  * 
  * The buffer is typically filled by a `b_gen` OSC message. (e.g. `buf.sine1(...)`
  * , `buf.sine2(...)` etc.)
  * 
  * ===Examples===
  * 
  * {{{
  * // sine1 example
  * val b = Buffer.alloc(s, 512)
  * b.sine1(partials = (1 to 6).map(1.0f / _),
  *   normalize = true, wavetable = true, clear = true)
  * 
  * play {
  *   Osc.ar(b.id, 200) * 0.3
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.OscN$ OscN]]
  * @see [[de.sciss.synth.ugen.COsc$ COsc]]
  * @see [[de.sciss.synth.ugen.VOsc$ VOsc]]
  * @see [[de.sciss.synth.ugen.SinOsc$ SinOsc]]
  */
object Osc {
  /** @param buf              the buffer with the wavetable in special wavetable
    *                         format. the size must be a power of two.
    * @param freq             frequency of table scans in Hz, corresponding to the
    *                         fundamental frequency of the sound.
    * @param phase            phase offset or modulator in radians. The value should
    *                         be within the range of -8*Pi to +8*Pi.
    */
  def kr(buf: GE, freq: GE = 440.0f, phase: GE = 0.0f): Osc = new Osc(control, buf, freq, phase)
  
  /** @param buf              the buffer with the wavetable in special wavetable
    *                         format. the size must be a power of two.
    * @param freq             frequency of table scans in Hz, corresponding to the
    *                         fundamental frequency of the sound.
    * @param phase            phase offset or modulator in radians. The value should
    *                         be within the range of -8*Pi to +8*Pi.
    */
  def ar(buf: GE, freq: GE = 440.0f, phase: GE = 0.0f): Osc = new Osc(audio, buf, freq, phase)
}

/** An oscillator UGen that linearly interpolates a wavetable. It has frequency and
  * phase modulation inputs. The wave table is provided by a buffer filled with a
  * wavetable format signal. The buffer size must be a power of 2.
  * 
  * The buffer is typically filled by a `b_gen` OSC message. (e.g. `buf.sine1(...)`
  * , `buf.sine2(...)` etc.)
  * 
  * @param buf              the buffer with the wavetable in special wavetable
  *                         format. the size must be a power of two.
  * @param freq             frequency of table scans in Hz, corresponding to the
  *                         fundamental frequency of the sound.
  * @param phase            phase offset or modulator in radians. The value should
  *                         be within the range of -8*Pi to +8*Pi.
  * 
  * @see [[de.sciss.synth.ugen.OscN$ OscN]]
  * @see [[de.sciss.synth.ugen.COsc$ COsc]]
  * @see [[de.sciss.synth.ugen.VOsc$ VOsc]]
  * @see [[de.sciss.synth.ugen.SinOsc$ SinOsc]]
  */
final case class Osc(rate: Rate, buf: GE, freq: GE = 440.0f, phase: GE = 0.0f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, freq.expand, phase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object OscN {
  def kr(buf: GE, freq: GE = 440.0f, phase: GE = 0.0f): OscN = new OscN(control, buf, freq, phase)
  
  def ar(buf: GE, freq: GE = 440.0f, phase: GE = 0.0f): OscN = new OscN(audio, buf, freq, phase)
}
final case class OscN(rate: Rate, buf: GE, freq: GE = 440.0f, phase: GE = 0.0f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, freq.expand, phase.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object COsc {
  def ar(buf: GE, freq: GE = 440.0f, beats: GE = 0.5f): COsc = new COsc(audio, buf, freq, beats)
}
final case class COsc(rate: Rate, buf: GE, freq: GE = 440.0f, beats: GE = 0.5f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, freq.expand, beats.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen that generates a set of harmonics around a formant frequency at a given
  * fundamental frequency.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulate fundamental frequency
  * play { Formant.ar(XLine.kr(400, 1000, 8), 2000, 800) * 0.2 }
  * }}}
  * {{{
  * // modulate formant frequency
  * play { Formant.ar(200, XLine.kr(400, 4000, 8), 200) * 0.2 }
  * }}}
  * {{{
  * // modulate the bandwidth
  * play { Formant.ar(400, 2000, XLine.kr(800, 8000, 8)) * 0.2 }
  * }}}
  */
object Formant {
  def ar: Formant = ar()
  
  /** @param fundFreq         Fundamental frequency in Hertz. Read at control-rate,
    *                         so if input is audio-rate, it will be sub-sampled.
    * @param formFreq         Formant frequency in Hertz. This determines the
    *                         overtone(s) most prominently perceived. Read at
    *                         control-rate, so if input is audio-rate, it will be
    *                         sub-sampled.
    * @param bw               Pulse width frequency in Hertz. Controls the bandwidth
    *                         of the formant. Must be greater than or equal to
    *                         `fundFreq` . Read at control-rate, so if input is
    *                         audio-rate, it will be sub-sampled.
    */
  def ar(fundFreq: GE = 440.0f, formFreq: GE = 1760.0f, bw: GE = 880.0f): Formant = 
    new Formant(audio, fundFreq, formFreq, bw)
}

/** A UGen that generates a set of harmonics around a formant frequency at a given
  * fundamental frequency.
  * 
  * @param fundFreq         Fundamental frequency in Hertz. Read at control-rate,
  *                         so if input is audio-rate, it will be sub-sampled.
  * @param formFreq         Formant frequency in Hertz. This determines the
  *                         overtone(s) most prominently perceived. Read at
  *                         control-rate, so if input is audio-rate, it will be
  *                         sub-sampled.
  * @param bw               Pulse width frequency in Hertz. Controls the bandwidth
  *                         of the formant. Must be greater than or equal to
  *                         `fundFreq` . Read at control-rate, so if input is
  *                         audio-rate, it will be sub-sampled.
  */
final case class Formant(rate: Rate, fundFreq: GE = 440.0f, formFreq: GE = 1760.0f, bw: GE = 880.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](fundFreq.expand, formFreq.expand, bw.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** Band Limited ImPulse generator UGen. All harmonics have equal amplitude. This
  * is the equivalent of 'buzz' in Music-N languages. It is capable of cross-fading
  * during a control period block if the number of harmonics changes, avoiding
  * audible pops.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulate fundamental frequency
  * play { Blip.ar(XLine.kr(20000, 200, 6), 100) * 0.2 }
  * }}}
  * {{{
  * // modulate number of harmonics
  * play { Blip.ar(200, Line.kr(1, 100, 20)) * 0.2 }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Impulse$ Impulse]]
  */
object Blip {
  def kr: Blip = kr()
  
  /** @param freq             Fundamental frequency in Hertz
    * @param numHarm          Number of harmonics. This will be automatically limited
    *                         to avoid aliasing.
    */
  def kr(freq: GE = 440.0f, numHarm: GE = 200): Blip = new Blip(control, freq, numHarm)
  
  def ar: Blip = ar()
  
  /** @param freq             Fundamental frequency in Hertz
    * @param numHarm          Number of harmonics. This will be automatically limited
    *                         to avoid aliasing.
    */
  def ar(freq: GE = 440.0f, numHarm: GE = 200): Blip = new Blip(audio, freq, numHarm)
}

/** Band Limited ImPulse generator UGen. All harmonics have equal amplitude. This
  * is the equivalent of 'buzz' in Music-N languages. It is capable of cross-fading
  * during a control period block if the number of harmonics changes, avoiding
  * audible pops.
  * 
  * @param freq             Fundamental frequency in Hertz
  * @param numHarm          Number of harmonics. This will be automatically limited
  *                         to avoid aliasing.
  * 
  * @see [[de.sciss.synth.ugen.Impulse$ Impulse]]
  */
final case class Blip(rate: Rate, freq: GE = 440.0f, numHarm: GE = 200) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, numHarm.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A band-limited sawtooth wave generator UGen.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulate frequency
  * play { Saw.ar(XLine.kr(40, 4000, 6)) * 0.2 }
  * }}}
  * {{{
  * // two saws with different frequencies through resonant filter
  * play { RLPF.ar(Saw.ar(Seq(100, 250)) * 0.2, XLine.kr(8000, 400, 6), 0.05) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  */
object Saw {
  def kr: Saw = kr()
  
  /** @param freq             Fundamental frequency in Hertz
    */
  def kr(freq: GE = 440.0f): Saw = new Saw(control, freq)
  
  def ar: Saw = ar()
  
  /** @param freq             Fundamental frequency in Hertz
    */
  def ar(freq: GE = 440.0f): Saw = new Saw(audio, freq)
}

/** A band-limited sawtooth wave generator UGen.
  * 
  * @param freq             Fundamental frequency in Hertz
  * 
  * @see [[de.sciss.synth.ugen.LFSaw$ LFSaw]]
  */
final case class Saw(rate: Rate, freq: GE = 440.0f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}

/** A band-limited pulse wave generator UGen, capable of pulse width modulation.
  * 
  * '''Note''': The fine behavior depends on the server's block-size which
  * interacts with the pulse width. The behavior is more regular if the duty period
  * `sr / freq * width` is an integer multiple of the block-size. A width of `1.0`
  * can produce strange results.
  * 
  * ===Examples===
  * 
  * {{{
  * // modulate frequency
  * play { Pulse.ar(XLine.kr(40, 4000, 6)) * 0.2 }
  * }}}
  * {{{
  * // modulate pulse width
  * play { Pulse.ar(200, Line.kr(0.01, 0.99, 8)) * 0.2 }
  * }}}
  * {{{
  * // two pulses with different frequencies through resonant filter
  * play { RLPF.ar(Pulse.ar(Seq(100, 250)) * 0.2, XLine.kr(8000, 400, 6), 0.05) }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.LFPulse$ LFPulse]]
  */
object Pulse {
  def kr: Pulse = kr()
  
  /** @param freq             Fundamental frequency in Hertz
    * @param width            Pulse width ratio from zero to one. `0.5` makes a
    *                         square wave.
    */
  def kr(freq: GE = 440.0f, width: GE = 0.5f): Pulse = new Pulse(control, freq, width)
  
  def ar: Pulse = ar()
  
  /** @param freq             Fundamental frequency in Hertz
    * @param width            Pulse width ratio from zero to one. `0.5` makes a
    *                         square wave.
    */
  def ar(freq: GE = 440.0f, width: GE = 0.5f): Pulse = new Pulse(audio, freq, width)
}

/** A band-limited pulse wave generator UGen, capable of pulse width modulation.
  * 
  * '''Note''': The fine behavior depends on the server's block-size which
  * interacts with the pulse width. The behavior is more regular if the duty period
  * `sr / freq * width` is an integer multiple of the block-size. A width of `1.0`
  * can produce strange results.
  * 
  * @param freq             Fundamental frequency in Hertz
  * @param width            Pulse width ratio from zero to one. `0.5` makes a
  *                         square wave.
  * 
  * @see [[de.sciss.synth.ugen.LFPulse$ LFPulse]]
  */
final case class Pulse(rate: Rate, freq: GE = 440.0f, width: GE = 0.5f) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](freq.expand, width.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}
object Klang {
  def ar(specs: GE, freqScale: GE = 1.0f, freqOffset: GE = 0.0f): Klang = 
    new Klang(specs, freqScale, freqOffset)
}
final case class Klang(specs: GE, freqScale: GE = 1.0f, freqOffset: GE = 0.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freqScale.expand, freqOffset.expand).++(specs.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, audio, _args)
}

/** Klank is a UGen of a bank of fixed frequency resonators which can be used to
  * simulate the resonant modes of an object. Each mode is given a ring time, which
  * is the time for the mode to decay by 60 dB.
  * 
  * ''Note'': `Ringz` and derived UGens `Klank` and `Formlet` produce output RMS
  * depending on the server's sampling rate. This is to achieve the same amplitude
  * for single-sample impulse inputs.
  * 
  * @see [[de.sciss.synth.ugen.Klang$ Klang]]
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  */
object Klank {
  /** @param specs             ''(init-time only)''
    * @param freqScale         ''(init-time only)''
    * @param freqOffset        ''(init-time only)''
    * @param decayScale        ''(init-time only)''
    */
  def ar(specs: GE, in: GE, freqScale: GE = 1.0f, freqOffset: GE = 0.0f, decayScale: GE = 1.0f): Klank = 
    new Klank(specs, in, freqScale, freqOffset, decayScale)
}

/** Klank is a UGen of a bank of fixed frequency resonators which can be used to
  * simulate the resonant modes of an object. Each mode is given a ring time, which
  * is the time for the mode to decay by 60 dB.
  * 
  * ''Note'': `Ringz` and derived UGens `Klank` and `Formlet` produce output RMS
  * depending on the server's sampling rate. This is to achieve the same amplitude
  * for single-sample impulse inputs.
  * 
  * @param specs             ''(init-time only)''
  * @param freqScale         ''(init-time only)''
  * @param freqOffset        ''(init-time only)''
  * @param decayScale        ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.Klang$ Klang]]
  * @see [[de.sciss.synth.ugen.Ringz$ Ringz]]
  */
final case class Klank(specs: GE, in: GE, freqScale: GE = 1.0f, freqOffset: GE = 0.0f, decayScale: GE = 1.0f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freqScale.expand, freqOffset.expand, decayScale.expand).++(specs.expand.outputs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, audio, _args)
}