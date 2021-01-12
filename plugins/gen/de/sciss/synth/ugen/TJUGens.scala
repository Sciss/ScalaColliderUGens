// revision: 3
package de.sciss.synth
package ugen

import UGenSource._

/** A digital filter UGen which aims at accurately modeling an analog filter. It
  * provides low-pass and high-pass modes, and the filter can be overdriven and will
  * self-oscillate at high resonances.
  * 
  * This is a third-party UGen (TJUGens).
  */
object DFM1 extends ProductReader[DFM1] {
  /** @param in               Input signal to filter.
    * @param freq             Cutoff frequency in Hertz.
    * @param res              Resonance of the filter. Resonance is minimal at `0.0`
    *                         and high at `1.0` , above which the filter starts
    *                         overdrive and sound saturated (e.g. `1.2` ).
    * @param gain             Linear gain applied to the input signal.
    * @param mode             The filter can be used in low-pass ( `0` ) or high-pass
    *                         ( `1` ) mode.
    * @param noise            Amount (amplitude) of noise added to the model.
    */
  def ar(in: GE, freq: GE = 440, res: GE = 0.1f, gain: GE = 1.0f, mode: GE = 0, noise: GE = 3.0E-4f): DFM1 = 
    new DFM1(audio, in, freq, res, gain, mode, noise)
  
  def read(in: RefMapIn, key: String, arity: Int): DFM1 = {
    require (arity == 7)
    val _rate   = in.readRate()
    val _in     = in.readGE()
    val _freq   = in.readGE()
    val _res    = in.readGE()
    val _gain   = in.readGE()
    val _mode   = in.readGE()
    val _noise  = in.readGE()
    new DFM1(_rate, _in, _freq, _res, _gain, _mode, _noise)
  }
}

/** A digital filter UGen which aims at accurately modeling an analog filter. It
  * provides low-pass and high-pass modes, and the filter can be overdriven and will
  * self-oscillate at high resonances.
  * 
  * This is a third-party UGen (TJUGens).
  * 
  * @param in               Input signal to filter.
  * @param freq             Cutoff frequency in Hertz.
  * @param res              Resonance of the filter. Resonance is minimal at `0.0`
  *                         and high at `1.0` , above which the filter starts
  *                         overdrive and sound saturated (e.g. `1.2` ).
  * @param gain             Linear gain applied to the input signal.
  * @param mode             The filter can be used in low-pass ( `0` ) or high-pass
  *                         ( `1` ) mode.
  * @param noise            Amount (amplitude) of noise added to the model.
  */
final case class DFM1(rate: Rate, in: GE, freq: GE = 440, res: GE = 0.1f, gain: GE = 1.0f, mode: GE = 0, noise: GE = 3.0E-4f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, freq.expand, res.expand, gain.expand, mode.expand, noise.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args)
}