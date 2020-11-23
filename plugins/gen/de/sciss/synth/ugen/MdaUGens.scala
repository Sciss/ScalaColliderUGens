// revision: 2
package de.sciss.synth
package ugen

import UGenSource._

/** A piano synthesiser UGen. It is not polyphonic, but it can be retriggered to
  * play notes in sequence.
  * 
  * The original VST plugin by Paul Kellett was ported to SuperCollider by Dan
  * Stowell. Most likely the arguments are in the normalized range 0 to 1.
  * 
  * This is a third-party UGen (MdaUGens).
  */
object MdaPiano {
  def ar: MdaPiano = ar()
  
  /** @param freq             Frequency of the note in Hz.
    * @param gate             note-on occurs when gate goes from non-positive to
    *                         positive; note-off occurs when it goes from positive to
    *                         non-positive. Most of the other controls are only
    *                         updated when a new note-on occurs.
    * @param velocity         velocity (range is 0 to 127)
    * @param decay            The time for notes to decay after the initial strike.
    * @param release          The time for notes to decay after the key is released.
    * @param hardness         adjusts sample key-ranges up or down to change the
    *                         "size" and brightness of the piano.
    * @param muffle           gentle low pass filter.
    * @param stereo           Width of the stereo effect (which makes low notes sound
    *                         towards the left, high notes towards the right). 0 to 1.
    * @param tune             Overall tuning.
    * @param random           Randomness in note tuning.
    * @param stretch          Stretches the tuning out (higher notes pushed higher).
    * @param sustain          if positive, act as if the piano's sustain pedal is
    *                         pressed.
    */
  def ar(freq: GE = 440, gate: GE = 1, velocity: GE = 100, decay: GE = 0.8f, release: GE = 0.8f, hardness: GE = 0.8f, velHard: GE = 0.8f, muffle: GE = 0.8f, velMuff: GE = 0.8f, velCurve: GE = 0.8f, stereo: GE = 0.2f, tune: GE = 0.5f, random: GE = 0.1f, stretch: GE = 0.1f, sustain: GE = 0): MdaPiano = 
    new MdaPiano(audio, freq, gate, velocity, decay, release, hardness, velHard, muffle, velMuff, velCurve, stereo, tune, random, stretch, sustain)
}

/** A piano synthesiser UGen. It is not polyphonic, but it can be retriggered to
  * play notes in sequence.
  * 
  * The original VST plugin by Paul Kellett was ported to SuperCollider by Dan
  * Stowell. Most likely the arguments are in the normalized range 0 to 1.
  * 
  * This is a third-party UGen (MdaUGens).
  * 
  * @param freq             Frequency of the note in Hz.
  * @param gate             note-on occurs when gate goes from non-positive to
  *                         positive; note-off occurs when it goes from positive to
  *                         non-positive. Most of the other controls are only
  *                         updated when a new note-on occurs.
  * @param velocity         velocity (range is 0 to 127)
  * @param decay            The time for notes to decay after the initial strike.
  * @param release          The time for notes to decay after the key is released.
  * @param hardness         adjusts sample key-ranges up or down to change the
  *                         "size" and brightness of the piano.
  * @param muffle           gentle low pass filter.
  * @param stereo           Width of the stereo effect (which makes low notes sound
  *                         towards the left, high notes towards the right). 0 to 1.
  * @param tune             Overall tuning.
  * @param random           Randomness in note tuning.
  * @param stretch          Stretches the tuning out (higher notes pushed higher).
  * @param sustain          if positive, act as if the piano's sustain pedal is
  *                         pressed.
  */
final case class MdaPiano(rate: Rate, freq: GE = 440, gate: GE = 1, velocity: GE = 100, decay: GE = 0.8f, release: GE = 0.8f, hardness: GE = 0.8f, velHard: GE = 0.8f, muffle: GE = 0.8f, velMuff: GE = 0.8f, velCurve: GE = 0.8f, stereo: GE = 0.2f, tune: GE = 0.5f, random: GE = 0.1f, stretch: GE = 0.1f, sustain: GE = 0)
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](freq.expand, gate.expand, velocity.expand, decay.expand, release.expand, hardness.expand, velHard.expand, muffle.expand, velMuff.expand, velCurve.expand, stereo.expand, tune.expand, random.expand, stretch.expand, sustain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.MultiOut(name, rate, Vector.fill(2)(rate), _args)
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}