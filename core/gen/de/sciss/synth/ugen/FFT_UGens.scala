// revision: 5
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen performing short-time forward fourier transformations. In order to
  * properly link the spectral ugens ( `PV_...` ), you should begin by using the
  * output of each UGen (which is just the fft buffer identifier), and use that as
  * buffer input of the next UGen. That way, the UGen graph is correctly sorted.
  * E.g. `IFFT(PV_...(FFT(buf, in)))` .
  * 
  * The UGen will initially output zero until the first FFT can be performed. This
  * is the case after `hop * fftSize` . Thus for a default fft buffer size of 1024
  * and a `hop` of 0.5, and for a default control block size of 64, for the first
  * 1024*0.5/64 = 8 control blocks the UGen will output zero. This also implies that
  * the first FFT in this case if performed on the first 512 samples of the `in`
  * signal (prepended by 512 zeros). In other words, the first 'full' FFT of the
  * input happens after fftSize/controlBlockSize cycles, no matter what hop size was
  * chosen.
  * 
  * If you use FFT for performing signal analysis and not phase vocoder effects,
  * make sure you change the window type accordingly.
  * 
  * ===Examples===
  * 
  * {{{
  * // perfect reconstruction
  * play {
  *   // with a hop of 0.5 and forward Hann window,
  *   // we get a perfect reconstruction delayed
  *   // by the fftSize minus one control-block.
  *   // (alternatively, you can use a hop of 1.0,
  *   //  and winType of 1 for both FFT and IFFT)
  *   val n     = 1024
  *   val hop   = 0.5
  *   val buf   = LocalBuf(n)
  *   val in    = PinkNoise.ar(0.5)
  *   val fft   = FFT(buf, in, hop = hop, winType = 1)
  *   val out   = IFFT.ar(fft, winType = -1)
  *   val dur   = (2 * n * hop) / SampleRate.ir - ControlDur.ir
  *   val dly   = DelayN.ar(in, dur, dur)
  *   out - dly
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.IFFT$ IFFT]]
  * @see [[de.sciss.synth.ugen.FFTTrigger$ FFTTrigger]]
  */
object FFT extends ProductReader[FFT] {
  def read(in: RefMapIn, prefix: String, arity: Int): FFT = {
    require (arity == 6)
    val _buf      = in.readGE()
    val _in       = in.readGE()
    val _hop      = in.readGE()
    val _winType  = in.readGE()
    val _active   = in.readGE()
    val _winSize  = in.readGE()
    new FFT(_buf, _in, _hop, _winType, _active, _winSize)
  }
}

/** A UGen performing short-time forward fourier transformations. In order to
  * properly link the spectral ugens ( `PV_...` ), you should begin by using the
  * output of each UGen (which is just the fft buffer identifier), and use that as
  * buffer input of the next UGen. That way, the UGen graph is correctly sorted.
  * E.g. `IFFT(PV_...(FFT(buf, in)))` .
  * 
  * The UGen will initially output zero until the first FFT can be performed. This
  * is the case after `hop * fftSize` . Thus for a default fft buffer size of 1024
  * and a `hop` of 0.5, and for a default control block size of 64, for the first
  * 1024*0.5/64 = 8 control blocks the UGen will output zero. This also implies that
  * the first FFT in this case if performed on the first 512 samples of the `in`
  * signal (prepended by 512 zeros). In other words, the first 'full' FFT of the
  * input happens after fftSize/controlBlockSize cycles, no matter what hop size was
  * chosen.
  * 
  * If you use FFT for performing signal analysis and not phase vocoder effects,
  * make sure you change the window type accordingly.
  * 
  * @param buf              The buffer to use for writing the FFT to. The size must
  *                         be a power of two. Since `FFT` operates at control rate
  *                         (also being a power of two), the buffer should probably
  *                         be at least as long as the control block size.
  *                         ''(init-time only)''
  * @param in               The time domain signal to be transformed into the
  *                         spectral domain.
  * @param hop              A factor determining the step size between successive
  *                         FFTs. That is, FFTs are performed every fftSize * hop
  *                         sample frames. The default of 0.5 means thus a 50%
  *                         overlap, while a hope of 1.0 means no overlapping.
  *                         Choosing 0.0 will most likely crash the server!
  *                         ''(init-time only)''
  * @param winType          The window function applied before each FFT is taken.
  *                         The default of `0` is a sine window which is good for
  *                         phase vocoder applications (using the `PV_...` UGens).
  *                         For analysis applications, you may want to use `-1`
  *                         which is a rectangle window (effectively no windowing)
  *                         or `1` which is a Hann window. A Hann window gives
  *                         perfect overlap-add reconstruction for a hope size of
  *                         0.5 (or 0.25 etc.) ''(init-time only)''
  * @param active           This parameter can be temporarily set to <= 0 to pause
  *                         the FFT operation.
  * @param winSize          With the default value of zero, the window size equals
  *                         the fft size. If you wish to perform zero padding, an
  *                         explicit window size can be specified. ''(init-time
  *                         only)''
  * 
  * @see [[de.sciss.synth.ugen.IFFT$ IFFT]]
  * @see [[de.sciss.synth.ugen.FFTTrigger$ FFTTrigger]]
  */
final case class FFT(buf: GE, in: GE, hop: GE = 0.5f, winType: GE = 0, active: GE = 1, winSize: GE = 0)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](buf.expand, in.expand, hop.expand, winType.expand, active.expand, winSize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A UGen performing an inverse FFT, transforming a buffer containing a spectral
  * domain signal back into the time domain.
  * 
  * ===Examples===
  * 
  * {{{
  * // perfect reconstruction
  * play {
  *   // with a hop of 0.5 and forward Hann window,
  *   // we get a perfect reconstruction delayed
  *   // by the fftSize minus one control-block.
  *   // (alternatively, you can use a hop of 1.0,
  *   //  and winType of 1 for both FFT and IFFT)
  *   val n     = 1024
  *   val hop   = 0.5
  *   val buf   = LocalBuf(n)
  *   val in    = PinkNoise.ar(0.5)
  *   val fft   = FFT(buf, in, hop = hop, winType = 1)
  *   val out   = IFFT.ar(fft, winType = -1)
  *   val dur   = (2 * n * hop) / SampleRate.ir - ControlDur.ir
  *   val dly   = DelayN.ar(in, dur, dur)
  *   out - dly
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FFT$ FFT]]
  */
object IFFT extends ProductReader[IFFT] {
  /** @param chain            reference to the spectral signal, returned as a
    *                         buffer-identifier from `FFT` or the `PV_...` UGens.
    * @param winType          The window function applied after each IFFT is taken.
    *                         The default of `0` is a sine window, `-1` is a rectangle
    *                         window, and `1` is a Hann window. ''(init-time only)''
    * @param winSize          With the default value of zero, the window size equals
    *                         the fft size. If you wish to perform zero padding, an
    *                         explicit window size can be specified. ''(init-time
    *                         only)''
    */
  def kr(chain: GE, winType: GE = 0, winSize: GE = 0): IFFT = new IFFT(control, chain, winType, winSize)
  
  /** @param chain            reference to the spectral signal, returned as a
    *                         buffer-identifier from `FFT` or the `PV_...` UGens.
    * @param winType          The window function applied after each IFFT is taken.
    *                         The default of `0` is a sine window, `-1` is a rectangle
    *                         window, and `1` is a Hann window. ''(init-time only)''
    * @param winSize          With the default value of zero, the window size equals
    *                         the fft size. If you wish to perform zero padding, an
    *                         explicit window size can be specified. ''(init-time
    *                         only)''
    */
  def ar(chain: GE, winType: GE = 0, winSize: GE = 0): IFFT = new IFFT(audio, chain, winType, winSize)
  
  def read(in: RefMapIn, prefix: String, arity: Int): IFFT = {
    require (arity == 4)
    val _rate     = in.readRate()
    val _chain    = in.readGE()
    val _winType  = in.readGE()
    val _winSize  = in.readGE()
    new IFFT(_rate, _chain, _winType, _winSize)
  }
}

/** A UGen performing an inverse FFT, transforming a buffer containing a spectral
  * domain signal back into the time domain.
  * 
  * @param chain            reference to the spectral signal, returned as a
  *                         buffer-identifier from `FFT` or the `PV_...` UGens.
  * @param winType          The window function applied after each IFFT is taken.
  *                         The default of `0` is a sine window, `-1` is a rectangle
  *                         window, and `1` is a Hann window. ''(init-time only)''
  * @param winSize          With the default value of zero, the window size equals
  *                         the fft size. If you wish to perform zero padding, an
  *                         explicit window size can be specified. ''(init-time
  *                         only)''
  * 
  * @see [[de.sciss.synth.ugen.FFT$ FFT]]
  */
final case class IFFT(rate: Rate, chain: GE, winType: GE = 0, winSize: GE = 0)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, winType.expand, winSize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}

/** A phase vocoder UGen that takes a buffer and prepares it to be used in FFT
  * chains, without doing an actual FFT on a signal. This is useful if you want to
  * provide a buffer whose content had already been transformed into the Fourier
  * domain.
  */
object FFTTrigger extends ProductReader[FFTTrigger] {
  def read(in: RefMapIn, prefix: String, arity: Int): FFTTrigger = {
    require (arity == 3)
    val _buf    = in.readGE()
    val _hop    = in.readGE()
    val _polar  = in.readGE()
    new FFTTrigger(_buf, _hop, _polar)
  }
}

/** A phase vocoder UGen that takes a buffer and prepares it to be used in FFT
  * chains, without doing an actual FFT on a signal. This is useful if you want to
  * provide a buffer whose content had already been transformed into the Fourier
  * domain.
  * 
  * @param buf              the identifier of the buffer to use ''(init-time only)''
  * @param hop              the hop size for timing triggers ''(init-time only)''
  * @param polar            whether the complex buffer content is given in
  *                         cartesian coordinates (0) or in polar coordinates (1)
  *                         ''(init-time only)''
  */
final case class FFTTrigger(buf: GE, hop: GE = 0.5f, polar: GE = 0)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](buf.expand, hop.expand, polar.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that passes only those bins whose magnitudes are above a
  * given threshold.
  */
object PV_MagAbove extends ProductReader[PV_MagAbove] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagAbove = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _thresh = in.readGE()
    new PV_MagAbove(_chain, _thresh)
  }
}

/** A phase vocoder UGen that passes only those bins whose magnitudes are above a
  * given threshold.
  * 
  * @param chain            the FFT'ed buffer
  * @param thresh           magnitude threshold.
  */
final case class PV_MagAbove(chain: GE, thresh: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, thresh.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that passes only those bins whose magnitudes are below a
  * given threshold.
  */
object PV_MagBelow extends ProductReader[PV_MagBelow] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagBelow = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _thresh = in.readGE()
    new PV_MagBelow(_chain, _thresh)
  }
}

/** A phase vocoder UGen that passes only those bins whose magnitudes are below a
  * given threshold.
  * 
  * @param chain            the FFT'ed buffer
  * @param thresh           magnitude threshold.
  */
final case class PV_MagBelow(chain: GE, thresh: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, thresh.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that limits (clips) the magnitude of the bins to a given
  * threshold.
  */
object PV_MagClip extends ProductReader[PV_MagClip] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagClip = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _thresh = in.readGE()
    new PV_MagClip(_chain, _thresh)
  }
}

/** A phase vocoder UGen that limits (clips) the magnitude of the bins to a given
  * threshold.
  * 
  * @param chain            the FFT'ed buffer
  * @param thresh           magnitude threshold. Each bin's magnitude is limited to
  *                         be not greater than this threshold.
  */
final case class PV_MagClip(chain: GE, thresh: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, thresh.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that multiplies the magnitudes of two inputs and keeps the
  * phases of the first input.
  */
object PV_MagMul extends ProductReader[PV_MagMul] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagMul = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_MagMul(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that multiplies the magnitudes of two inputs and keeps the
  * phases of the first input.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_MagMul(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that divides magnitudes of two inputs and keeps the phases
  * of the first input.
  */
object PV_MagDiv extends ProductReader[PV_MagDiv] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagDiv = {
    require (arity == 3)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    val _zeroes = in.readGE()
    new PV_MagDiv(_chainA, _chainB, _zeroes)
  }
}

/** A phase vocoder UGen that divides magnitudes of two inputs and keeps the phases
  * of the first input.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  * @param zeroes           the noise floor to assume when detecting zero bins in
  *                         chainB that would cause a division by zero and hence
  *                         blow up. The ugen will use divide by this magnitude
  *                         instead when zeroes are detected, resulting in a maximum
  *                         boost of `zeroes.reciprocal` .
  */
final case class PV_MagDiv(chainA: GE, chainB: GE, zeroes: GE = 1.0E-4f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand, zeroes.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that squares the magnitudes and re-normalizes to previous
  * peak. This makes weak bins weaker.
  */
object PV_MagSquared extends ProductReader[PV_MagSquared] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagSquared = {
    require (arity == 1)
    val _chain = in.readGE()
    new PV_MagSquared(_chain)
  }
}

/** A phase vocoder UGen that squares the magnitudes and re-normalizes to previous
  * peak. This makes weak bins weaker.
  * 
  * @param chain            the FFT'ed buffer
  */
final case class PV_MagSquared(chain: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that multiplies the magnitudes by random noise.
  */
object PV_MagNoise extends ProductReader[PV_MagNoise] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagNoise = {
    require (arity == 1)
    val _chain = in.readGE()
    new PV_MagNoise(_chain)
  }
}

/** A phase vocoder UGen that multiplies the magnitudes by random noise.
  * 
  * @param chain            the FFT'ed buffer
  */
final case class PV_MagNoise(chain: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that copies the spectral frames from chainA to chainB.
  * This allows for parallel processing of spectral data without the need for
  * multiple FFT UGens, and to copy out data at that point in the chain for other
  * purposes. chainA and chainB must be the same size. The output will carry further
  * chainA, so you chan insert the ugen at the appropriate place in the signal
  * chain.
  */
object PV_Copy extends ProductReader[PV_Copy] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Copy = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_Copy(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that copies the spectral frames from chainA to chainB.
  * This allows for parallel processing of spectral data without the need for
  * multiple FFT UGens, and to copy out data at that point in the chain for other
  * purposes. chainA and chainB must be the same size. The output will carry further
  * chainA, so you chan insert the ugen at the appropriate place in the signal
  * chain.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_Copy(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that combines the magnitudes of first input and phases of
  * the second input. phases of the first input.
  */
object PV_CopyPhase extends ProductReader[PV_CopyPhase] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_CopyPhase = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_CopyPhase(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that combines the magnitudes of first input and phases of
  * the second input. phases of the first input.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_CopyPhase(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that shifts the phase of each bins by a given amount.
  */
object PV_PhaseShift extends ProductReader[PV_PhaseShift] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_PhaseShift = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _shift  = in.readGE()
    new PV_PhaseShift(_chain, _shift)
  }
}

/** A phase vocoder UGen that shifts the phase of each bins by a given amount.
  * 
  * @param chain            the FFT'ed buffer
  * @param shift            phase shift in radians
  */
final case class PV_PhaseShift(chain: GE, shift: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, shift.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that shift the phase of all bins by 90 degrees.
  */
object PV_PhaseShift90 extends ProductReader[PV_PhaseShift90] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_PhaseShift90 = {
    require (arity == 1)
    val _chain = in.readGE()
    new PV_PhaseShift90(_chain)
  }
}

/** A phase vocoder UGen that shift the phase of all bins by 90 degrees.
  * 
  * @param chain            the FFT'ed buffer
  */
final case class PV_PhaseShift90(chain: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that shift the phase of all bins by 270 (or -90) degrees.
  */
object PV_PhaseShift270 extends ProductReader[PV_PhaseShift270] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_PhaseShift270 = {
    require (arity == 1)
    val _chain = in.readGE()
    new PV_PhaseShift270(_chain)
  }
}

/** A phase vocoder UGen that shift the phase of all bins by 270 (or -90) degrees.
  * 
  * @param chain            the FFT'ed buffer
  */
final case class PV_PhaseShift270(chain: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that outputs the bins with the minimum magnitude of the
  * two inputs.
  */
object PV_Min extends ProductReader[PV_Min] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Min = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_Min(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that outputs the bins with the minimum magnitude of the
  * two inputs.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_Min(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that outputs the bins with the maximum magnitude of the
  * two inputs.
  */
object PV_Max extends ProductReader[PV_Max] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Max = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_Max(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that outputs the bins with the maximum magnitude of the
  * two inputs.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_Max(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that performs a complex multiplication of the two inputs.
  * The formula is
  * `(Re(A) * Re(B) - Im(A) * Im(B)) + i(Im(A) * Re(B) + Re(A) * Im(B))` .
  */
object PV_Mul extends ProductReader[PV_Mul] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Mul = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_Mul(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that performs a complex multiplication of the two inputs.
  * The formula is
  * `(Re(A) * Re(B) - Im(A) * Im(B)) + i(Im(A) * Re(B) + Re(A) * Im(B))` .
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_Mul(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that performs a complex division of the two inputs. Be
  * careful that `chainB` , the divisor, does not contain zeroes as they would
  * obviously blow up the division.
  */
object PV_Div extends ProductReader[PV_Div] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Div = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_Div(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that performs a complex division of the two inputs. Be
  * careful that `chainB` , the divisor, does not contain zeroes as they would
  * obviously blow up the division.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_Div(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that performs a complex addition of the two inputs. The
  * formula is `(Re(A) + Re(B)) + i(Im(A) + Im(B))` .
  */
object PV_Add extends ProductReader[PV_Add] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Add = {
    require (arity == 2)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    new PV_Add(_chainA, _chainB)
  }
}

/** A phase vocoder UGen that performs a complex addition of the two inputs. The
  * formula is `(Re(A) + Re(B)) + i(Im(A) + Im(B))` .
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  */
final case class PV_Add(chainA: GE, chainB: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that makes a series of gaps in a spectrum. This is done by
  * multiplying the spectrum with a kind of rectangle wave that goes from zero to
  * nyquist. The high slope of the rectangle lets the input bins pass (quasi
  * pass-band), the low slope filters them out (quasi stop-band).
  * 
  * @see [[de.sciss.synth.ugen.PV_RectComb2$ PV_RectComb2]]
  */
object PV_RectComb extends ProductReader[PV_RectComb] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_RectComb = {
    require (arity == 4)
    val _chain    = in.readGE()
    val _numTeeth = in.readGE()
    val _phase    = in.readGE()
    val _width    = in.readGE()
    new PV_RectComb(_chain, _numTeeth, _phase, _width)
  }
}

/** A phase vocoder UGen that makes a series of gaps in a spectrum. This is done by
  * multiplying the spectrum with a kind of rectangle wave that goes from zero to
  * nyquist. The high slope of the rectangle lets the input bins pass (quasi
  * pass-band), the low slope filters them out (quasi stop-band).
  * 
  * @param chain            the FFT'ed buffer
  * @param numTeeth         the number of periods in the rectangle wave, where zero
  *                         would mean the input signal is not affected, one means
  *                         that there is exactly one period of the wave across the
  *                         spectrum, hence one pass-band and one stop-band.
  * @param phase            the phase offset of the rectangle wave, where 1.0 is
  *                         one full period. This is like the offset into the
  *                         wavetable holding the rectangle, so a value of 0.25
  *                         means we start 25% into the basic waveform, and after
  *                         0.75 periods the next full period (high slope) begins.
  * @param width            the pulse width between 0.0 (infinitely small high
  *                         slope, so all bins filtered out) to 0.5 (half period is
  *                         high slope, half period is low slope) to 1.0 (maximally
  *                         wide high slope, no bins filtered out).
  * 
  * @see [[de.sciss.synth.ugen.PV_RectComb2$ PV_RectComb2]]
  */
final case class PV_RectComb(chain: GE, numTeeth: GE = 1.0f, phase: GE = 0.0f, width: GE = 0.5f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, numTeeth.expand, phase.expand, width.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that switches between two input spectra according to a
  * rectangle wave. This is basically identical to `PV_RectComb` , however during
  * the low slopes of the rectangle wave, instead of clearing out the bins, it
  * copies over the corresponding bins of the second fft input buffer.
  * 
  * @see [[de.sciss.synth.ugen.PV_RectComb$ PV_RectComb]]
  */
object PV_RectComb2 extends ProductReader[PV_RectComb2] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_RectComb2 = {
    require (arity == 5)
    val _chainA   = in.readGE()
    val _chainB   = in.readGE()
    val _numTeeth = in.readGE()
    val _phase    = in.readGE()
    val _width    = in.readGE()
    new PV_RectComb2(_chainA, _chainB, _numTeeth, _phase, _width)
  }
}

/** A phase vocoder UGen that switches between two input spectra according to a
  * rectangle wave. This is basically identical to `PV_RectComb` , however during
  * the low slopes of the rectangle wave, instead of clearing out the bins, it
  * copies over the corresponding bins of the second fft input buffer.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  * @param numTeeth         the number of periods in the rectangle wave, where zero
  *                         would mean the first input signal is fully passed
  *                         through, one means that there is exactly one period of
  *                         the wave across the spectrum, hence one pass-band (first
  *                         signal passed through) and one stop-band (second signal
  *                         passed through).
  * @param phase            the phase offset of the rectangle wave, where 1.0 is
  *                         one full period. This is like the offset into the
  *                         wavetable holding the rectangle, so a value of 0.25
  *                         means we start 25% into the basic waveform, and after
  *                         0.75 periods the next full period (high slope) begins.
  * @param width            the pulse width between 0.0 (infinitely small high
  *                         slope, so all bins are copied from the second input) to
  *                         0.5 (half period is high slope -- copied from first
  *                         input --, half period is low slope -- copied from second
  *                         input) to 1.0 (maximally wide high slope, so all bins
  *                         passed from the first input).
  * 
  * @see [[de.sciss.synth.ugen.PV_RectComb$ PV_RectComb]]
  */
final case class PV_RectComb2(chainA: GE, chainB: GE, numTeeth: GE = 1.0f, phase: GE = 0.0f, width: GE = 0.5f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand, numTeeth.expand, phase.expand, width.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that clears bins above or below a cutoff point.
  */
object PV_BrickWall extends ProductReader[PV_BrickWall] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_BrickWall = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _wipe   = in.readGE()
    new PV_BrickWall(_chain, _wipe)
  }
}

/** A phase vocoder UGen that clears bins above or below a cutoff point.
  * 
  * @param chain            the FFT'ed buffer
  * @param wipe             can range between -1 and +1. if wipe == 0 then there is
  *                         no effect. if wipe > 0 then it acts like a high pass
  *                         filter, clearing bins from the bottom up. if wipe < 0
  *                         then it acts like a low pass filter, clearing bins from
  *                         the top down.
  */
final case class PV_BrickWall(chain: GE, wipe: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, wipe.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that combine low and high bins from two inputs. It does so
  * by copying low bins from one input and the high bins of the other, thus realizes
  * a kind of "wipe" between the two input signals.
  * 
  * @see [[de.sciss.synth.ugen.PV_RandWipe$ PV_RandWipe]]
  */
object PV_BinWipe extends ProductReader[PV_BinWipe] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_BinWipe = {
    require (arity == 3)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    val _wipe   = in.readGE()
    new PV_BinWipe(_chainA, _chainB, _wipe)
  }
}

/** A phase vocoder UGen that combine low and high bins from two inputs. It does so
  * by copying low bins from one input and the high bins of the other, thus realizes
  * a kind of "wipe" between the two input signals.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  * @param wipe             can range between -1 and +1. if wipe == 0 then the
  *                         output is the same as inA. if wipe > 0 then it begins
  *                         replacing with bins from inB from the bottom up. if wipe
  *                         < 0 then it begins replacing with bins from inB from the
  *                         top down.
  * 
  * @see [[de.sciss.synth.ugen.PV_RandWipe$ PV_RandWipe]]
  */
final case class PV_BinWipe(chainA: GE, chainB: GE, wipe: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand, wipe.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that passes only those bins whose magnitudes constitute
  * local maxima. Additionally, the given threshold is also used to filter out bins
  * whose magnitude lies below this threshold.
  */
object PV_LocalMax extends ProductReader[PV_LocalMax] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_LocalMax = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _thresh = in.readGE()
    new PV_LocalMax(_chain, _thresh)
  }
}

/** A phase vocoder UGen that passes only those bins whose magnitudes constitute
  * local maxima. Additionally, the given threshold is also used to filter out bins
  * whose magnitude lies below this threshold.
  * 
  * @param chain            the FFT'ed buffer
  * @param thresh           magnitude threshold used for general filtering, prior
  *                         to the local-maximum-filtering
  */
final case class PV_LocalMax(chain: GE, thresh: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, thresh.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that converts the bins into their complex conjugate
  * counterparts. The complex conjugate is equal to the input, but with reversed
  * sign of the imaginary part.
  */
object PV_Conj extends ProductReader[PV_Conj] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Conj = {
    require (arity == 1)
    val _chain = in.readGE()
    new PV_Conj(_chain)
  }
}

/** A phase vocoder UGen that converts the bins into their complex conjugate
  * counterparts. The complex conjugate is equal to the input, but with reversed
  * sign of the imaginary part.
  * 
  * @param chain            the FFT'ed buffer
  */
final case class PV_Conj(chain: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that randomizes the order of the bins. The trigger will
  * select a new random ordering.
  */
object PV_BinScramble extends ProductReader[PV_BinScramble] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_BinScramble = {
    require (arity == 4)
    val _chain  = in.readGE()
    val _wipe   = in.readGE()
    val _width  = in.readGE()
    val _trig   = in.readGE()
    new PV_BinScramble(_chain, _wipe, _width, _trig)
  }
}

/** A phase vocoder UGen that randomizes the order of the bins. The trigger will
  * select a new random ordering.
  * 
  * @param chain            the FFT'ed buffer
  * @param wipe             the amount of bins scrambled, from 0 (none) to 1 (all
  *                         bins scrambled).
  * @param width            a value from zero to one, indicating the maximum
  *                         randomized distance of a bin from its original location
  *                         in the spectrum.
  * @param trig             causes a new random bin re-ordering to be made. a
  *                         trigger occurs when passing from non-positive to
  *                         positive value.
  */
final case class PV_BinScramble(chain: GE, wipe: GE = 0.5f, width: GE = 0.2f, trig: GE = 1)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, wipe.expand, width.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that averages each bin's magnitude with its neighbors.
  */
object PV_MagSmear extends ProductReader[PV_MagSmear] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagSmear = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _bins   = in.readGE()
    new PV_MagSmear(_chain, _bins)
  }
}

/** A phase vocoder UGen that averages each bin's magnitude with its neighbors.
  * 
  * @param chain            the FFT'ed buffer
  * @param bins             number of bins to average on each side of bin. As this
  *                         number rises, so will CPU usage.
  */
final case class PV_MagSmear(chain: GE, bins: GE = 1)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, bins.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that stretches and shifts the magnitudes of the spectrum.
  * This is live `PV_BinShift` but instead of scaling and shifting the whole complex
  * bins (magnitude and phase), this only operates on the magnitudes and leaves the
  * phases in their original bins.
  */
object PV_MagShift extends ProductReader[PV_MagShift] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagShift = {
    require (arity == 3)
    val _chain    = in.readGE()
    val _stretch  = in.readGE()
    val _shift    = in.readGE()
    new PV_MagShift(_chain, _stretch, _shift)
  }
}

/** A phase vocoder UGen that stretches and shifts the magnitudes of the spectrum.
  * This is live `PV_BinShift` but instead of scaling and shifting the whole complex
  * bins (magnitude and phase), this only operates on the magnitudes and leaves the
  * phases in their original bins.
  * 
  * @param chain            the FFT'ed buffer
  * @param stretch          the factor to multiply each bin position with
  * @param shift            the translation of the spectrum, in number of bins
  */
final case class PV_MagShift(chain: GE, stretch: GE = 1.0f, shift: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, stretch.expand, shift.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that stretches and shifts the spectrum. It takes each bin,
  * first stretches (scales) its position (bin number) with a given factor, and then
  * adds a shift to it.
  */
object PV_BinShift extends ProductReader[PV_BinShift] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_BinShift = {
    require (arity == 3)
    val _chain    = in.readGE()
    val _stretch  = in.readGE()
    val _shift    = in.readGE()
    new PV_BinShift(_chain, _stretch, _shift)
  }
}

/** A phase vocoder UGen that stretches and shifts the spectrum. It takes each bin,
  * first stretches (scales) its position (bin number) with a given factor, and then
  * adds a shift to it.
  * 
  * @param chain            the FFT'ed buffer
  * @param stretch          the factor to multiply each bin position with, where
  *                         0.5 kind of transposes the signal down by an octave, and
  *                         2 transposes it up by an octave.
  * @param shift            the translation of the spectrum, in number of bins.
  *                         Since the FFT produces a linear frequency axis, the will
  *                         produce harmonic distortion.
  */
final case class PV_BinShift(chain: GE, stretch: GE = 1.0f, shift: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, stretch.expand, shift.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that cross-fades between two input spectra by taking bins
  * randomly from them according to a given probability.
  * 
  * @see [[de.sciss.synth.ugen.PV_BinWipe$ PV_BinWipe]]
  */
object PV_RandWipe extends ProductReader[PV_RandWipe] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_RandWipe = {
    require (arity == 4)
    val _chainA = in.readGE()
    val _chainB = in.readGE()
    val _wipe   = in.readGE()
    val _trig   = in.readGE()
    new PV_RandWipe(_chainA, _chainB, _wipe, _trig)
  }
}

/** A phase vocoder UGen that cross-fades between two input spectra by taking bins
  * randomly from them according to a given probability.
  * 
  * @param chainA           the first FFT'ed buffer (this gets replaced by the
  *                         output signal)
  * @param chainB           the second FFT'ed buffer
  * @param wipe             the crossfader position from 0.0 (all bins are taken
  *                         from `chainA` ) to 1.0 (all bins are taken from `chainB`
  *                         ). For instance, if wipe is 0.5, half of the bins are
  *                         taken from either input. The decision whether a bin is
  *                         taken from A or B is random, however remains constant
  *                         between two triggers.
  * @param trig             a signal the triggers the re-newed process of
  *                         determining for each bin whether it will be taken from
  *                         input A or B. A trigger occurs when passing from
  *                         non-positive to positive value.
  * 
  * @see [[de.sciss.synth.ugen.PV_BinWipe$ PV_BinWipe]]
  */
final case class PV_RandWipe(chainA: GE, chainB: GE, wipe: GE, trig: GE)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chainA.expand, chainB.expand, wipe.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that adds a different constant random phase shift to each
  * bin. The trigger will select a new set of random phases.
  */
object PV_Diffuser extends ProductReader[PV_Diffuser] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_Diffuser = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _trig   = in.readGE()
    new PV_Diffuser(_chain, _trig)
  }
}

/** A phase vocoder UGen that adds a different constant random phase shift to each
  * bin. The trigger will select a new set of random phases.
  * 
  * @param chain            the FFT'ed buffer
  * @param trig             to trigger a new selection of random phases. A trigger
  *                         occurs when passing from non-positive to positive value.
  */
final case class PV_Diffuser(chain: GE, trig: GE = 1)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that randomly clears out bins of the signal. Which bins
  * are wiped out is subject to a random choice (only the amount is specified) that
  * remains constant between triggers.
  */
object PV_RandComb extends ProductReader[PV_RandComb] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_RandComb = {
    require (arity == 3)
    val _chain  = in.readGE()
    val _wipe   = in.readGE()
    val _trig   = in.readGE()
    new PV_RandComb(_chain, _wipe, _trig)
  }
}

/** A phase vocoder UGen that randomly clears out bins of the signal. Which bins
  * are wiped out is subject to a random choice (only the amount is specified) that
  * remains constant between triggers.
  * 
  * @param chain            the FFT'ed buffer
  * @param wipe             the probability (from 0 to 1) of bins being wiped out,
  *                         hence 0 means no bins are wiped out, 1 means all bins
  *                         are wiped out (output will be silence).
  * @param trig             causes a new random bin selection to be made. a trigger
  *                         occurs when passing from non-positive to positive value.
  */
final case class PV_RandComb(chain: GE, wipe: GE = 0.5f, trig: GE = 1)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, wipe.expand, trig.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A phase vocoder UGen that freezes the magnitudes at current levels. Freezing
  * happens when the freeze input has a value of > 0.
  */
object PV_MagFreeze extends ProductReader[PV_MagFreeze] {
  def read(in: RefMapIn, prefix: String, arity: Int): PV_MagFreeze = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _freeze = in.readGE()
    new PV_MagFreeze(_chain, _freeze)
  }
}

/** A phase vocoder UGen that freezes the magnitudes at current levels. Freezing
  * happens when the freeze input has a value of > 0.
  * 
  * @param chain            the FFT'ed buffer
  * @param freeze           whether the current levels are frozen (> 0) or not (0).
  */
final case class PV_MagFreeze(chain: GE, freeze: GE = 1)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, freeze.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}

/** A UGen for partitioned convolution. Its advantage over non-partitioning UGens
  * such as `Convolution2` is that the impulse response can be arbitrarily large
  * amortization is used to spread processing and avoid CPU spikes.
  * 
  * The impulse response buffer must be specially prepared, using a `/b_gen`
  * command that transforms an existing regularly formatted buffer to a new
  * partitioned convolution ready buffer.
  * 
  * ===Examples===
  * 
  * {{{
  * // Dan Stowell's reverb
  * // synthesize impulse response
  * val ir = (1f +: Vector.fill(100)(0f)) ++ (1f to 0f by -0.00002f).map { f =>
  *   if (math.random < 0.5)
  *     0f
  *   else
  *     f.pow(8) * (math.random - 0.5).signum * 0.1f
  * }
  * 
  * // ir.plot()
  * 
  * // send the IR to a regular buffer
  * val irBuf = Buffer(s)
  * irBuf.alloc(ir.size)
  * irBuf.setData(ir)
  * 
  * // calculate the partitioning parameters
  * val fftSize  = 2048
  * val numPart  = (ir.size * 2.0 / fftSize).ceil.toInt  // 49
  * val partSize = fftSize * numPart  // 100352
  * 
  * // create the specially formatted partitioned buffer
  * val partBuf  = Buffer(s)
  * partBuf.alloc(partSize)
  * // currently no predefined method for this command!
  * s ! osc.Message("/b_gen", partBuf.id, "PreparePartConv", irBuf.id, fftSize)
  * 
  * // now we can forget about the input buffer
  * irBuf.free()
  * 
  * val x = play {
  *   // trigger IR every 4 seconds
  *   val in = Impulse.ar(0.25) * 0.5
  *   PartConv.ar(in, fftSize, partBuf.id)
  * }
  * 
  * // do not forget to free the buffer eventually
  * x.free(); partBuf.free()
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  */
object PartConv extends ProductReader[PartConv] {
  /** @param in               the realtime input to be convolved
    * @param fftSize          FFT size which is twice the input signal partition
    *                         size. This must be a multiple of the control-block size,
    *                         and there must be at least two blocks per partition (to
    *                         allow for amortization) ''(init-time only)''
    * @param buf              buffer identifier for the fixed kernel (init-time
    *                         only). ''(init-time only)''
    */
  def ar(in: GE, fftSize: GE, buf: GE): PartConv = new PartConv(audio, in, fftSize, buf)
  
  def read(in: RefMapIn, prefix: String, arity: Int): PartConv = {
    require (arity == 4)
    val _rate     = in.readRate()
    val _in       = in.readGE()
    val _fftSize  = in.readGE()
    val _buf      = in.readGE()
    new PartConv(_rate, _in, _fftSize, _buf)
  }
}

/** A UGen for partitioned convolution. Its advantage over non-partitioning UGens
  * such as `Convolution2` is that the impulse response can be arbitrarily large
  * amortization is used to spread processing and avoid CPU spikes.
  * 
  * The impulse response buffer must be specially prepared, using a `/b_gen`
  * command that transforms an existing regularly formatted buffer to a new
  * partitioned convolution ready buffer.
  * 
  * @param in               the realtime input to be convolved
  * @param fftSize          FFT size which is twice the input signal partition
  *                         size. This must be a multiple of the control-block size,
  *                         and there must be at least two blocks per partition (to
  *                         allow for amortization) ''(init-time only)''
  * @param buf              buffer identifier for the fixed kernel (init-time
  *                         only). ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.Convolution$ Convolution]]
  * @see [[de.sciss.synth.ugen.Convolution2$ Convolution2]]
  */
final case class PartConv(rate: Rate, in: GE, fftSize: GE, buf: GE)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, fftSize.expand, buf.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}