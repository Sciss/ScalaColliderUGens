// revision: 3
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen that performs a convolution with an continuously changing kernel. If the
  * kernel is static or must only change occasionally, `Convolution2` will be a more
  * CPU friendly alternative. The process introduces a delay of
  * `frameSize - blockSize` .
  * 
  * ===Examples===
  * 
  * {{{
  * // sine filter
  * play {
  *   val a = WhiteNoise.ar
  *   val b = SinOsc.ar(MouseY.kr(20, 2000, 1))
  *   Convolution.ar(a, b, 512) * 0.01
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  */
object Convolution extends Reader[Convolution] {
  /** @param in               first operand of the convolution
    * @param kernel           second operand of the convolution
    * @param frameSize        convolution size in sample frames, which is half of the
    *                         FFT size. Must be a power of two. There is maximum
    *                         frame-size of 16384 (if exceeded, the server may crash).
    *                         ''(init-time only)''
    */
  def ar(in: GE, kernel: GE, frameSize: GE): Convolution = new Convolution(audio, in, kernel, frameSize)
  
  def read(in: DataInput): Convolution = {
    readArity(in, 4)
    val _rate       = readRate(in)
    val _in         = readGE(in)
    val _kernel     = readGE(in)
    val _frameSize  = readGE(in)
    new Convolution(_rate, _in, _kernel, _frameSize)
  }
}

/** A UGen that performs a convolution with an continuously changing kernel. If the
  * kernel is static or must only change occasionally, `Convolution2` will be a more
  * CPU friendly alternative. The process introduces a delay of
  * `frameSize - blockSize` .
  * 
  * @param in               first operand of the convolution
  * @param kernel           second operand of the convolution
  * @param frameSize        convolution size in sample frames, which is half of the
  *                         FFT size. Must be a power of two. There is maximum
  *                         frame-size of 16384 (if exceeded, the server may crash).
  *                         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  */
final case class Convolution(rate: Rate, in: GE, kernel: GE, frameSize: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, kernel.expand, frameSize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A frequency-domain convolution UGen using a fixed kernel which can be updated
  * by a trigger signal. The delay caused by the convolution when the kernel is a
  * dirac impulse is equal to `frameSize - controlBlockSize` , so for a `frameSize`
  * of 2048 and a control-block size of 64, this is 1984 sample frames.
  * 
  * ===Examples===
  * 
  * {{{
  * // three example kernels
  * // creates a buffer with `n` set values
  * def mkBuf(n: Int, amp: => Double): Buffer = {
  *   val v = Vector.tabulate[FillValue](n) { i =>
  *     (i.linLin(0, n, 0, 2048).toInt, amp)
  *   }
  *   val b = Buffer(s)
  *   b.alloc(2048, completion = b.zeroMsg(b.setMsg(v: _*)))
  *   b
  * }
  * 
  * val b = mkBuf(3, 1)
  * val c = mkBuf(50, math.random)
  * val d = mkBuf(20, 1)
  * 
  * val x = play {
  *   val z   = Impulse.ar(1)
  *   val buf = "kernel".kr(b.id)
  *   val tr  = "trig"  .tr
  *   Convolution2.ar(z, buf, tr, 2048) * 0.5
  * }
  * 
  * // set buffer and trigger kernel actualization
  * x.set("kernel" -> b.id, "trig" -> 1)
  * x.set("kernel" -> c.id, "trig" -> 1)
  * x.set("kernel" -> d.id, "trig" -> 1)
  * 
  * x.free(); b.free(); c.free(); d.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  */
object Convolution2 extends Reader[Convolution2] {
  /** @param in               the realtime input to be convolved
    * @param kernel           buffer identifier for the fixed kernel, which may be
    *                         modulated in combination with the trigger. Even a
    *                         trigger input of zero is used, upon UGen initialization
    *                         the kernel must point to a valid buffer, otherwise the
    *                         UGen aborts.
    * @param trig             updates the kernel on a change from non-positive to
    *                         positive (<= 0 to >0)
    * @param frameSize        size of the kernel. this must be a power of two. the
    *                         FFT calculated internally by the UGen has a size of
    *                         twice this value. The maximum allowed `frameSize` is
    *                         16384. ''(init-time only)''
    */
  def ar(in: GE, kernel: GE, trig: GE = 1, frameSize: GE): Convolution2 = 
    new Convolution2(audio, in, kernel, trig, frameSize)
  
  def read(in: DataInput): Convolution2 = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _in         = readGE(in)
    val _kernel     = readGE(in)
    val _trig       = readGE(in)
    val _frameSize  = readGE(in)
    new Convolution2(_rate, _in, _kernel, _trig, _frameSize)
  }
}

/** A frequency-domain convolution UGen using a fixed kernel which can be updated
  * by a trigger signal. The delay caused by the convolution when the kernel is a
  * dirac impulse is equal to `frameSize - controlBlockSize` , so for a `frameSize`
  * of 2048 and a control-block size of 64, this is 1984 sample frames.
  * 
  * @param in               the realtime input to be convolved
  * @param kernel           buffer identifier for the fixed kernel, which may be
  *                         modulated in combination with the trigger. Even a
  *                         trigger input of zero is used, upon UGen initialization
  *                         the kernel must point to a valid buffer, otherwise the
  *                         UGen aborts.
  * @param trig             updates the kernel on a change from non-positive to
  *                         positive (<= 0 to >0)
  * @param frameSize        size of the kernel. this must be a power of two. the
  *                         FFT calculated internally by the UGen has a size of
  *                         twice this value. The maximum allowed `frameSize` is
  *                         16384. ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  */
final case class Convolution2(rate: Rate, in: GE, kernel: GE, trig: GE = 1, frameSize: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, kernel.expand, trig.expand, frameSize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A frequency-domain convolution UGen using two linearly interpolated fixed
  * kernels. When a trigger is received, a linear fade will be performed from the
  * previously used kernel (internally stored by the UGen) towards the snapshot of
  * the current kernel content upon receiving the trigger.
  * 
  * The delay caused by the convolution when the kernel is a dirac impulse is equal
  * to `frameSize - controlBlockSize` , so for a `frameSize` of 2048 and a
  * control-block size of 64, this is 1984 sample frames.
  * 
  * '''Note''': If a trigger is received before the previous fade is complete, the
  * interpolation is broken and the kernel instead jumps straight to one of the two
  * buffers.
  * 
  * ===Examples===
  * 
  * {{{
  * // three example kernels
  * def mkBuf(n: Int, amp: => Double): Buffer = {
  *   val v = Vector.tabulate[FillValue](n) { i =>
  *     (i.linLin(0, n, 0, 2048).toInt, amp)
  *   }
  *   val b = Buffer(s)
  *   b.alloc(2048, completion = b.zeroMsg(b.setMsg(v: _*)))
  *   b
  * }
  * 
  * val b = mkBuf(3, 1)
  * val c = mkBuf(50, math.random)
  * val d = mkBuf(20, 1)
  * 
  * val x = play {
  *   val z     = Impulse.ar(16)
  *   val buf   = "kernel".kr(b.id)
  *   val tr    = "trig"  .tr
  *   val dur   = 4.0          // fade-time in seconds
  *   val n     = 2048
  *   val block = SampleRate.ir / n
  *   val p     = dur * block  // ... in periods
  *   Convolution2L.ar(z, buf, tr, 2048, p) * 0.5
  * }
  * 
  * x.set("kernel" -> b.id, "trig" -> 1)
  * x.set("kernel" -> c.id, "trig" -> 1)
  * x.set("kernel" -> d.id, "trig" -> 1)
  * 
  * x.free(); b.free(); c.free(); d.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  */
object Convolution2L extends Reader[Convolution2L] {
  /** @param in               the realtime input to be convolved
    * @param kernel           buffer identifier for the fixed kernel, which may be
    *                         modulated in combination with the trigger. Even if the
    *                         trigger input is initially zero, upon UGen
    *                         initialization the kernel must point to a valid buffer,
    *                         otherwise the UGen aborts.
    * @param trig             begins a fade to update the kernel on a change from
    *                         non-positive to positive (<= 0 to >0).
    * @param frameSize        size of the kernel. this must be a power of two. the
    *                         FFT calculated internally by the UGen has a size of
    *                         twice this value. The maximum allowed `frameSize` is
    *                         16384. ''(init-time only)''
    * @param fadePeriods      fade duration expressed as number of convolved blocks.
    *                         If the desired duration in seconds is `dur` , then the
    *                         number of periods can be calculated as
    *                         `fadePeriods = dur * SampleRate.ir / frameSize` .
    *                         ''(init-time only)''
    */
  def ar(in: GE, kernel: GE, trig: GE = 1, frameSize: GE, fadePeriods: GE = 1): Convolution2L = 
    new Convolution2L(audio, in, kernel, trig, frameSize, fadePeriods)
  
  def read(in: DataInput): Convolution2L = {
    readArity(in, 6)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _kernel       = readGE(in)
    val _trig         = readGE(in)
    val _frameSize    = readGE(in)
    val _fadePeriods  = readGE(in)
    new Convolution2L(_rate, _in, _kernel, _trig, _frameSize, _fadePeriods)
  }
}

/** A frequency-domain convolution UGen using two linearly interpolated fixed
  * kernels. When a trigger is received, a linear fade will be performed from the
  * previously used kernel (internally stored by the UGen) towards the snapshot of
  * the current kernel content upon receiving the trigger.
  * 
  * The delay caused by the convolution when the kernel is a dirac impulse is equal
  * to `frameSize - controlBlockSize` , so for a `frameSize` of 2048 and a
  * control-block size of 64, this is 1984 sample frames.
  * 
  * '''Note''': If a trigger is received before the previous fade is complete, the
  * interpolation is broken and the kernel instead jumps straight to one of the two
  * buffers.
  * 
  * @param in               the realtime input to be convolved
  * @param kernel           buffer identifier for the fixed kernel, which may be
  *                         modulated in combination with the trigger. Even if the
  *                         trigger input is initially zero, upon UGen
  *                         initialization the kernel must point to a valid buffer,
  *                         otherwise the UGen aborts.
  * @param trig             begins a fade to update the kernel on a change from
  *                         non-positive to positive (<= 0 to >0).
  * @param frameSize        size of the kernel. this must be a power of two. the
  *                         FFT calculated internally by the UGen has a size of
  *                         twice this value. The maximum allowed `frameSize` is
  *                         16384. ''(init-time only)''
  * @param fadePeriods      fade duration expressed as number of convolved blocks.
  *                         If the desired duration in seconds is `dur` , then the
  *                         number of periods can be calculated as
  *                         `fadePeriods = dur * SampleRate.ir / frameSize` .
  *                         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  */
final case class Convolution2L(rate: Rate, in: GE, kernel: GE, trig: GE = 1, frameSize: GE, fadePeriods: GE = 1)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, kernel.expand, trig.expand, frameSize.expand, fadePeriods.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A frequency domain stereo convolution UGen, capable of performing linear
  * cross-fades between kernel updates. When receiving a trigger, there is a linear
  * cross-fade between the old kernel the new buffer contents. It operates similar
  * to `Convolution2L` , however uses two buffers and outputs a stereo signal,
  * resulting in better CPU usage than two discrete instances of `Convolution2L` as
  * this way one FFT transformation per period is saved.
  * 
  * '''Warning: This UGen seems currently broken (SC 3.6.3)'''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  */
object StereoConvolution2L extends Reader[StereoConvolution2L] {
  /** @param in               the realtime input to be convolved
    * @param kernelL          buffer identifier for the left channel's fixed kernel,
    *                         which may be modulated in combination with the trigger
    * @param kernelR          buffer identifier for the right channel's fixed kernel,
    *                         which may be modulated in combination with the trigger
    * @param trig             updates the kernel on a change from non-positive to
    *                         positive (<= 0 to >0), and starts a new cross-fade from
    *                         the previous kernel to the new one over the given amount
    *                         of periods.
    * @param frameSize        size of the kernel. this must be a power of two. the
    *                         FFT calculated internally by the UGen has a size of
    *                         twice this value. The maximum allowed `frameSize` is
    *                         16384. ''(init-time only)''
    * @param fadePeriods      fade duration expressed as number of convolved blocks.
    *                         If the desired duration in seconds is `dur` , then the
    *                         number of periods can be calculated as
    *                         `fadePeriods = dur * SampleRate.ir / frameSize` .
    *                         ''(init-time only)''
    */
  def ar(in: GE, kernelL: GE, kernelR: GE, trig: GE = 1, frameSize: GE, fadePeriods: GE = 1): StereoConvolution2L = 
    new StereoConvolution2L(audio, in, kernelL, kernelR, trig, frameSize, fadePeriods)
  
  def read(in: DataInput): StereoConvolution2L = {
    readArity(in, 7)
    val _rate         = readRate(in)
    val _in           = readGE(in)
    val _kernelL      = readGE(in)
    val _kernelR      = readGE(in)
    val _trig         = readGE(in)
    val _frameSize    = readGE(in)
    val _fadePeriods  = readGE(in)
    new StereoConvolution2L(_rate, _in, _kernelL, _kernelR, _trig, _frameSize, _fadePeriods)
  }
}

/** A frequency domain stereo convolution UGen, capable of performing linear
  * cross-fades between kernel updates. When receiving a trigger, there is a linear
  * cross-fade between the old kernel the new buffer contents. It operates similar
  * to `Convolution2L` , however uses two buffers and outputs a stereo signal,
  * resulting in better CPU usage than two discrete instances of `Convolution2L` as
  * this way one FFT transformation per period is saved.
  * 
  * '''Warning: This UGen seems currently broken (SC 3.6.3)'''
  * 
  * @param in               the realtime input to be convolved
  * @param kernelL          buffer identifier for the left channel's fixed kernel,
  *                         which may be modulated in combination with the trigger
  * @param kernelR          buffer identifier for the right channel's fixed kernel,
  *                         which may be modulated in combination with the trigger
  * @param trig             updates the kernel on a change from non-positive to
  *                         positive (<= 0 to >0), and starts a new cross-fade from
  *                         the previous kernel to the new one over the given amount
  *                         of periods.
  * @param frameSize        size of the kernel. this must be a power of two. the
  *                         FFT calculated internally by the UGen has a size of
  *                         twice this value. The maximum allowed `frameSize` is
  *                         16384. ''(init-time only)''
  * @param fadePeriods      fade duration expressed as number of convolved blocks.
  *                         If the desired duration in seconds is `dur` , then the
  *                         number of periods can be calculated as
  *                         `fadePeriods = dur * SampleRate.ir / frameSize` .
  *                         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  */
final case class StereoConvolution2L(rate: Rate, in: GE, kernelL: GE, kernelR: GE, trig: GE = 1, frameSize: GE, fadePeriods: GE = 1)
  extends UGenSource.MultiOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, kernelL.expand, kernelR.expand, trig.expand, frameSize.expand, fadePeriods.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args, isIndividual = true)
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** A UGen for triggered convolution in the time domain.
  * 
  * '''Warning: This UGen seems currently broken (SC 3.6.3)'''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  */
object Convolution3 extends Reader[Convolution3] {
  /** @param in               the realtime input to be convolved
    * @param kernel           buffer identifier for the fixed kernel, which may be
    *                         modulated in combination with the trigger. Even a
    *                         trigger input of zero is used, upon UGen initialization
    *                         the kernel must point to a valid buffer, otherwise the
    *                         UGen aborts.
    * @param frameSize         ''(init-time only)''
    */
  def ar(in: GE, kernel: GE, trig: GE = 1, frameSize: GE): Convolution3 = 
    new Convolution3(audio, in, kernel, trig, frameSize)
  
  def read(in: DataInput): Convolution3 = {
    readArity(in, 5)
    val _rate       = readRate(in)
    val _in         = readGE(in)
    val _kernel     = readGE(in)
    val _trig       = readGE(in)
    val _frameSize  = readGE(in)
    new Convolution3(_rate, _in, _kernel, _trig, _frameSize)
  }
}

/** A UGen for triggered convolution in the time domain.
  * 
  * '''Warning: This UGen seems currently broken (SC 3.6.3)'''
  * 
  * @param in               the realtime input to be convolved
  * @param kernel           buffer identifier for the fixed kernel, which may be
  *                         modulated in combination with the trigger. Even a
  *                         trigger input of zero is used, upon UGen initialization
  *                         the kernel must point to a valid buffer, otherwise the
  *                         UGen aborts.
  * @param frameSize         ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PartConv$ PartConv]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  * @see [[de.sciss.synth.ugen.Convolution2L$ Convolution2L]]
  */
final case class Convolution3(rate: Rate, in: GE, kernel: GE, trig: GE = 1, frameSize: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, kernel.expand, trig.expand, frameSize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen that applies the conformal mapping `z => (z - a) / (1 - za*)` to its
  * input FFT bins `z` .
  * 
  * It makes a transformation of the complex plane so the output is full of phase
  * vocoder artifacts but may be musically interesting. One should usually keep
  * `|a| < 1` , although bigger values may be used to produce noise. A value of
  * `a = 0` gives back the input mostly unperturbed.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse control
  * play {
  *   val sf   = Seq.fill(3)(Rand(0.1, 0.5))
  *   val sadd = Seq(1, 1.1, 1.5, 1.78, 2.45, 6.7).map(_ * 220)
  *   val in   = Mix(LFSaw.ar(SinOsc.kr(sf).mulAdd(10, sadd)) * 0.3)
  *   val fft  = FFT(LocalBuf(2048), in)
  *   val re   = MouseX.kr(0.01,  2.0, 1)
  *   val im   = MouseY.kr(0.01, 10.0, 1)
  *   val pv   = PV_ConformalMap(fft, re, im)
  *   val out  = IFFT.ar(pv)
  *   val vrb  = CombN.ar(out, 0.1, 0.1, 10)
  *   Pan2.ar(out + vrb * 0.5, 0, 0.3)
  * }
  * }}}
  */
object PV_ConformalMap extends Reader[PV_ConformalMap] {
  def read(in: DataInput): PV_ConformalMap = {
    readArity(in, 3)
    val _chain  = readGE(in)
    val _real   = readGE(in)
    val _imag   = readGE(in)
    new PV_ConformalMap(_chain, _real, _imag)
  }
}

/** A UGen that applies the conformal mapping `z => (z - a) / (1 - za*)` to its
  * input FFT bins `z` .
  * 
  * It makes a transformation of the complex plane so the output is full of phase
  * vocoder artifacts but may be musically interesting. One should usually keep
  * `|a| < 1` , although bigger values may be used to produce noise. A value of
  * `a = 0` gives back the input mostly unperturbed.
  * 
  * @param chain            the FFT'ed buffer
  * @param real             real part of the complex parameter `a`
  * @param imag             imaginary part of the complex parameter `a`
  */
final case class PV_ConformalMap(chain: GE, real: GE = 0.0f, imag: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, real.expand, imag.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** An FFT based onset detector UGen using a mix of extracted features. It is based
  * on work described in Jensen and Andersen (2003), "Real-time Beat Estimation
  * Using Feature Extraction," in: Proceedings of the Computer Music Modeling and
  * Retrieval Symposium.
  * 
  * First order derivatives of the features are taken. The threshold may need to be
  * set low to pick up on changes.
  * 
  * ===Examples===
  * 
  * {{{
  * // observe detection
  * play {
  *   val sig = Decay.ar(Dust.ar(2), 0.1) * WhiteNoise.ar(0.25)
  *   val th  = MouseX.kr(0.1, 1.0, lag = 0)
  *   th.poll(HPZ1.kr(th).abs, "thresh")
  *   val tr  = PV_JensenAndersen.ar(FFT(LocalBuf(2048), sig), thresh = th)
  *   Seq(sig, SinOsc.ar(440) * Decay.ar(tr * 0.01, 0.1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Onsets$ Onsets]]
  * @see [[de.sciss.synth.ugen.PV_HainsworthFoote$ PV_HainsworthFoote]]
  */
object PV_JensenAndersen extends Reader[PV_JensenAndersen] {
  /** @param chain            the fft signal (buffer) to analyze
    * @param centroid         proportion (0 to 1) of spectral centroid feature
    * @param hfe              proportion (0 to 1) of high frequency energy feature
    * @param hfc              proportion (0 to 1) of high frequency content feature
    * @param flux             proportion (0 to 1) of spectral flux feature
    * @param thresh           threshold level for detection
    * @param waitTime         after an onset is detected, further detections are
    *                         suppressed for this period in seconds, preventing
    *                         multiple rapid triggers
    */
  def ar(chain: GE, centroid: GE = 0.25f, hfe: GE = 0.25f, hfc: GE = 0.25f, flux: GE = 0.25f, thresh: GE = 1.0f, waitTime: GE = 0.04f): PV_JensenAndersen = 
    new PV_JensenAndersen(audio, chain, centroid, hfe, hfc, flux, thresh, waitTime)
  
  def read(in: DataInput): PV_JensenAndersen = {
    readArity(in, 8)
    val _rate     = readRate(in)
    val _chain    = readGE(in)
    val _centroid = readGE(in)
    val _hfe      = readGE(in)
    val _hfc      = readGE(in)
    val _flux     = readGE(in)
    val _thresh   = readGE(in)
    val _waitTime = readGE(in)
    new PV_JensenAndersen(_rate, _chain, _centroid, _hfe, _hfc, _flux, _thresh, _waitTime)
  }
}

/** An FFT based onset detector UGen using a mix of extracted features. It is based
  * on work described in Jensen and Andersen (2003), "Real-time Beat Estimation
  * Using Feature Extraction," in: Proceedings of the Computer Music Modeling and
  * Retrieval Symposium.
  * 
  * First order derivatives of the features are taken. The threshold may need to be
  * set low to pick up on changes.
  * 
  * @param chain            the fft signal (buffer) to analyze
  * @param centroid         proportion (0 to 1) of spectral centroid feature
  * @param hfe              proportion (0 to 1) of high frequency energy feature
  * @param hfc              proportion (0 to 1) of high frequency content feature
  * @param flux             proportion (0 to 1) of spectral flux feature
  * @param thresh           threshold level for detection
  * @param waitTime         after an onset is detected, further detections are
  *                         suppressed for this period in seconds, preventing
  *                         multiple rapid triggers
  * 
  * @see [[de.sciss.synth.ugen.Onsets$ Onsets]]
  * @see [[de.sciss.synth.ugen.PV_HainsworthFoote$ PV_HainsworthFoote]]
  */
final case class PV_JensenAndersen(rate: Rate, chain: GE, centroid: GE = 0.25f, hfe: GE = 0.25f, hfc: GE = 0.25f, flux: GE = 0.25f, thresh: GE = 1.0f, waitTime: GE = 0.04f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, centroid.expand, hfe.expand, hfc.expand, flux.expand, thresh.expand, waitTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** An FFT based onset detector UGen using a balance of two features. It is based
  * on work described in Hainsworth (2003), "Techniques for the Automated Analysis
  * of Musical Audio," PhD thesis, University of Cambridge. See especially p. 128.
  * The Hainsworth metric is a modification of the Kullback Liebler distance.
  * 
  * ===Examples===
  * 
  * {{{
  * // observe detection
  * play {
  *   val sig = Decay.ar(Dust.ar(2), 0.1) * WhiteNoise.ar(0.25)
  *   val th  = MouseX.kr(0.3, 1.0, lag = 0)
  *   th.poll(HPZ1.kr(th).abs, "thresh")
  *   val h   = MouseY.kr(1.0, 0.1, lag = 0)
  *   val f   = 1 - h
  *   h.poll(HPZ1.kr(h).abs, "h-f")
  *   val tr  = PV_HainsworthFoote.ar(FFT(LocalBuf(2048), sig), h, f, thresh = th)
  *   Seq(sig, SinOsc.ar(440) * Decay.ar(tr * 0.01, 0.1))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Onsets$ Onsets]]
  * @see [[de.sciss.synth.ugen.PV_JensenAndersen$ PV_JensenAndersen]]
  */
object PV_HainsworthFoote extends Reader[PV_HainsworthFoote] {
  /** @param chain            the fft signal (buffer) to analyze
    * @param hainsworth       what strength of detection signal from Hainsworth
    *                         metric (0 to 1) to use.
    * @param foote            what strength of detection signal from normalized Foote
    *                         metric (0 to 1) to use.
    * @param thresh           threshold level for detection
    * @param waitTime         after an onset is detected, further detections are
    *                         suppressed for this period in seconds, preventing
    *                         multiple rapid triggers
    */
  def ar(chain: GE, hainsworth: GE = 0.0f, foote: GE = 0.0f, thresh: GE = 1.0f, waitTime: GE = 0.04f): PV_HainsworthFoote = 
    new PV_HainsworthFoote(audio, chain, hainsworth, foote, thresh, waitTime)
  
  def read(in: DataInput): PV_HainsworthFoote = {
    readArity(in, 6)
    val _rate       = readRate(in)
    val _chain      = readGE(in)
    val _hainsworth = readGE(in)
    val _foote      = readGE(in)
    val _thresh     = readGE(in)
    val _waitTime   = readGE(in)
    new PV_HainsworthFoote(_rate, _chain, _hainsworth, _foote, _thresh, _waitTime)
  }
}

/** An FFT based onset detector UGen using a balance of two features. It is based
  * on work described in Hainsworth (2003), "Techniques for the Automated Analysis
  * of Musical Audio," PhD thesis, University of Cambridge. See especially p. 128.
  * The Hainsworth metric is a modification of the Kullback Liebler distance.
  * 
  * @param chain            the fft signal (buffer) to analyze
  * @param hainsworth       what strength of detection signal from Hainsworth
  *                         metric (0 to 1) to use.
  * @param foote            what strength of detection signal from normalized Foote
  *                         metric (0 to 1) to use.
  * @param thresh           threshold level for detection
  * @param waitTime         after an onset is detected, further detections are
  *                         suppressed for this period in seconds, preventing
  *                         multiple rapid triggers
  * 
  * @see [[de.sciss.synth.ugen.Onsets$ Onsets]]
  * @see [[de.sciss.synth.ugen.PV_JensenAndersen$ PV_JensenAndersen]]
  */
final case class PV_HainsworthFoote(rate: Rate, chain: GE, hainsworth: GE = 0.0f, foote: GE = 0.0f, thresh: GE = 1.0f, waitTime: GE = 0.04f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, hainsworth.expand, foote.expand, thresh.expand, waitTime.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A UGen calculating the sum of an input signal over a sliding window of given
  * number of samples.
  * 
  * ''Note'': Unlike `RunningMin` and `RunningMax` , this is not a trigger based
  * operation, but at any one time, the sum of the past `length` values is reported,
  * continuously sliding the analysis window.
  * 
  * ===Examples===
  * 
  * {{{
  * // poll waveform's DC offset
  * play {
  *   val freq = 441
  *   val n    = SampleRate.ir / freq
  *   // mean over period of a pulse with 50% duty is 0.5
  *   val mean = RunningSum.ar(LFPulse.ar(freq), n) / n
  *   mean.roundTo(0.01).poll(label = "mean")
  *   ()
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.Median$ Median]]
  * @see [[de.sciss.synth.ugen.PulseCount$ PulseCount]]
  */
object RunningSum extends Reader[RunningSum] {
  /** @param in               the input signal to sum up
    * @param length           the length of the sliding window over the input signal.
    *                         these are the number of audio sample-frames for
    *                         audio-rate calculation, or the number of blocks for
    *                         control-rate calculation summed up. ''Warning'': The
    *                         UGen crashes when length is zero. ''(init-time only)''
    */
  def kr(in: GE, length: GE = 440): RunningSum = new RunningSum(control, in, length)
  
  /** @param in               the input signal to sum up
    * @param length           the length of the sliding window over the input signal.
    *                         these are the number of audio sample-frames for
    *                         audio-rate calculation, or the number of blocks for
    *                         control-rate calculation summed up. ''Warning'': The
    *                         UGen crashes when length is zero. ''(init-time only)''
    */
  def ar(in: GE, length: GE = 440): RunningSum = new RunningSum(audio, in, length)
  
  def read(in: DataInput): RunningSum = {
    readArity(in, 3)
    val _rate   = readMaybeRate(in)
    val _in     = readGE(in)
    val _length = readGE(in)
    new RunningSum(_rate, _in, _length)
  }
}

/** A UGen calculating the sum of an input signal over a sliding window of given
  * number of samples.
  * 
  * ''Note'': Unlike `RunningMin` and `RunningMax` , this is not a trigger based
  * operation, but at any one time, the sum of the past `length` values is reported,
  * continuously sliding the analysis window.
  * 
  * @param in               the input signal to sum up
  * @param length           the length of the sliding window over the input signal.
  *                         these are the number of audio sample-frames for
  *                         audio-rate calculation, or the number of blocks for
  *                         control-rate calculation summed up. ''Warning'': The
  *                         UGen crashes when length is zero. ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.Integrator$ Integrator]]
  * @see [[de.sciss.synth.ugen.Median$ Median]]
  * @see [[de.sciss.synth.ugen.PulseCount$ PulseCount]]
  */
final case class RunningSum(rate: MaybeRate, in: GE, length: GE = 440) extends UGenSource.SingleOut {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, length.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}