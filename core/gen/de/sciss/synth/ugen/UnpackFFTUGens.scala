// revision: 7
package de.sciss.synth
package ugen

import UGenSource._
object Unpack1FFT extends ProductReader[Unpack1FFT] {
  def read(in: RefMapIn, arity: Int): Unpack1FFT = {
    require (arity == 4)
    val _chain    = in.readGE()
    val _fftSize  = in.readGE()
    val _bin      = in.readGE()
    val _which    = in.readGE()
    new Unpack1FFT(_chain, _fftSize, _bin, _which)
  }
}
final case class Unpack1FFT(chain: GE, fftSize: GE, bin: GE, which: GE = 0)
  extends UGenSource.SingleOut with DemandRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, fftSize.expand, bin.expand, which.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, demand, _args, isIndividual = true)
}

/** A UGen that writes a complex input signal into an FFT buffer. The input is a
  * sequence of interleaved magnitudes and phases. It is written to an FFT buffer
  * ready for transforming it back into time-domain audio using IFFT.
  * 
  * ===Examples===
  * 
  * {{{
  * // harmonic sound with changing overtone intensities
  * play {
  *   // create simple undulating magnitudes
  *   val m0 = FSinOsc.kr(Seq.fill(100)(ExpRand(0.1, 1))) * 0.5 + 0.5
  *   // give them a "rolloff" to make the sound less unpleasant
  *   val m1 = m0 * Seq.tabulate(100)(_.linLin(0, 99, 1.0, 0.01).squared)
  *   // turn the bins on and off at different rates
  *   val mags = m1 * LFPulse.kr(Seq.fill(100)(2 pow IRand(-3, 5)))
  *   // ignore phase
  *   val phases: GE = Seq.fill(100)(0)
  *   // We need to create an FFT chain to feed our data in to.
  *   // The easiest way is to do an FFT on some signal which we then ignore!
  *   val buf = FFT(LocalBuf(512), DC.ar(0))
  *   // now we can do the packing
  *   val chain = PackFFT(buf, 512, Zip(mags, phases), 0, 99, 1)
  *   val sig = IFFT.ar(chain)
  *   Pan2.ar(sig)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.Unpack1FFT$ Unpack1FFT]]
  * @see [[de.sciss.synth.ugen.FFT$ FFT]]
  * @see [[de.sciss.synth.ugen.IFFT$ IFFT]]
  * @see [[de.sciss.synth.ugen.SetBuf$ SetBuf]]
  */
object PackFFT extends ProductReader[PackFFT] {
  def read(in: RefMapIn, arity: Int): PackFFT = {
    require (arity == 6)
    val _chain    = in.readGE()
    val _fftSize  = in.readGE()
    val _values   = in.readGE()
    val _from     = in.readGE()
    val _to       = in.readGE()
    val _clear    = in.readGE()
    new PackFFT(_chain, _fftSize, _values, _from, _to, _clear)
  }
}

/** A UGen that writes a complex input signal into an FFT buffer. The input is a
  * sequence of interleaved magnitudes and phases. It is written to an FFT buffer
  * ready for transforming it back into time-domain audio using IFFT.
  * 
  * @param values           input data to pack. It should be a flat sequence
  *                         containing interleaved magnitude and phase components of
  *                         all bins in ascending order. E.g. [mag0, phase0, mag1,
  *                         phase1, mag2, phase2, ... magN, phaseN]. This input is
  *                         typically demand-rate.
  * @param from             index of lower bin
  * @param to               index of upper bin (inclusive)
  * @param clear            if `1` , clears the buffer before packing the values,
  *                         setting its contents to zero.
  * 
  * @see [[de.sciss.synth.ugen.Unpack1FFT$ Unpack1FFT]]
  * @see [[de.sciss.synth.ugen.FFT$ FFT]]
  * @see [[de.sciss.synth.ugen.IFFT$ IFFT]]
  * @see [[de.sciss.synth.ugen.SetBuf$ SetBuf]]
  */
final case class PackFFT(chain: GE, fftSize: GE, values: GE, from: GE = 0, to: GE, clear: GE = 0)
  extends UGenSource.SingleOut with ControlRated with HasSideEffect with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, fftSize.expand, from.expand, to.expand, clear.expand).++({
      val _exp = values.expand.outputs
      _exp.+:(Constant(_exp.size))
    }))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, hasSideEffect = true, isIndividual = true)
}