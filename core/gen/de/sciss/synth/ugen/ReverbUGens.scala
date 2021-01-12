// revision: 4
package de.sciss.synth
package ugen

import UGenSource._

/** A monophonic reverb UGen. All parameters are specified in and automatically
  * clipped to the range 0 to 1. The UGen is stateless insofar it does not use a
  * random number generator.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse control for mix and room
  * play {
  *   val in   = Decay.ar(Impulse.ar(1), 0.25) * LFCub.ar(1200) * 0.1
  *   val mix  = MouseX.kr
  *   val room = MouseY.kr
  *   val verb = FreeVerb.ar(in, mix, room, "damp".kr(0.5))
  *   Pan2.ar(verb)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb2$ FreeVerb2]]
  * @see [[de.sciss.synth.ugen.GVerb$ GVerb]]
  */
object FreeVerb extends ProductReader[FreeVerb] {
  /** @param in               input signal to reverberate
    * @param mix              dry/wet balance from zero (only dry) to one (only wet)
    * @param room             room size
    * @param damp             high frequency attenuation (1 is maximum attenuation)
    */
  def ar(in: GE, mix: GE = 0.33f, room: GE = 0.5f, damp: GE = 0.5f): FreeVerb = 
    new FreeVerb(in, mix, room, damp)
  
  def read(in: RefMapIn, key: String, arity: Int): FreeVerb = {
    require (arity == 4)
    val _in   = in.readGE()
    val _mix  = in.readGE()
    val _room = in.readGE()
    val _damp = in.readGE()
    new FreeVerb(_in, _mix, _room, _damp)
  }
}

/** A monophonic reverb UGen. All parameters are specified in and automatically
  * clipped to the range 0 to 1. The UGen is stateless insofar it does not use a
  * random number generator.
  * 
  * @param in               input signal to reverberate
  * @param mix              dry/wet balance from zero (only dry) to one (only wet)
  * @param room             room size
  * @param damp             high frequency attenuation (1 is maximum attenuation)
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb2$ FreeVerb2]]
  * @see [[de.sciss.synth.ugen.GVerb$ GVerb]]
  */
final case class FreeVerb(in: GE, mix: GE = 0.33f, room: GE = 0.5f, damp: GE = 0.5f)
  extends UGenSource.SingleOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, mix.expand, room.expand, damp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = matchRate(_args, 0, audio)
    UGen.SingleOut(name, audio, _args1)
  }
}

/** A stereophonic reverb UGen. All parameters are specified in and automatically
  * clipped to the range 0 to 1. The UGen is stateless insofar it does not use a
  * random number generator. However, if the same input is used for left and right
  * channel, the output channels are different and uncorrelated. There is also some
  * cross-feed between the two channels.
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse control for mix and room, random input pan
  * play {
  *   val tr   = Impulse.ar(1)
  *   val in   = Decay.ar(tr, 0.25) * LFCub.ar(1200) * 0.1
  *   val in2  = Pan2.ar(in, TRand.ar(-1, 1, tr))
  *   val mix  = MouseX.kr
  *   val room = MouseY.kr
  *   FreeVerb2.ar(in2 out 0, in2 out 1, mix, room, "damp".kr(0.5))
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb$ FreeVerb]]
  * @see [[de.sciss.synth.ugen.GVerb$ GVerb]]
  */
object FreeVerb2 extends ProductReader[FreeVerb2] {
  /** @param inL              left channel of input signal to reverberate
    * @param inR              right channel of input signal to reverberate
    * @param mix              dry/wet balance from zero (only dry) to one (only wet)
    * @param room             room size
    * @param damp             high frequency attenuation (1 is maximum attenuation)
    */
  def ar(inL: GE, inR: GE, mix: GE = 0.33f, room: GE = 0.5f, damp: GE = 0.5f): FreeVerb2 = 
    new FreeVerb2(inL, inR, mix, room, damp)
  
  def read(in: RefMapIn, key: String, arity: Int): FreeVerb2 = {
    require (arity == 5)
    val _inL  = in.readGE()
    val _inR  = in.readGE()
    val _mix  = in.readGE()
    val _room = in.readGE()
    val _damp = in.readGE()
    new FreeVerb2(_inL, _inR, _mix, _room, _damp)
  }
}

/** A stereophonic reverb UGen. All parameters are specified in and automatically
  * clipped to the range 0 to 1. The UGen is stateless insofar it does not use a
  * random number generator. However, if the same input is used for left and right
  * channel, the output channels are different and uncorrelated. There is also some
  * cross-feed between the two channels.
  * 
  * @param inL              left channel of input signal to reverberate
  * @param inR              right channel of input signal to reverberate
  * @param mix              dry/wet balance from zero (only dry) to one (only wet)
  * @param room             room size
  * @param damp             high frequency attenuation (1 is maximum attenuation)
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb$ FreeVerb]]
  * @see [[de.sciss.synth.ugen.GVerb$ GVerb]]
  */
final case class FreeVerb2(inL: GE, inR: GE, mix: GE = 0.33f, room: GE = 0.5f, damp: GE = 0.5f)
  extends UGenSource.MultiOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](inL.expand, inR.expand, mix.expand, room.expand, damp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (audio.==(audio)) matchRate(_args, 0, audio) else _args
    val _args2 = if (audio.==(audio)) matchRate(_args1, 1, audio) else _args1
    UGen.MultiOut(name, audio, Vector.fill(2)(audio), _args2)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}

/** A stereophonic reverb UGen. It is based on the GVerb LADSPA effect by Juhana
  * Sadeharju.
  * 
  * '''Note:''' A CPU spike may occur when the synth is instantiated.
  * '''Warning:''' The UGen has a bug which results in loud noise if the room size
  * is increased during playback. It seems safe to start with a large room size and
  * ''decrease'' the value during playing. '''Warning:''' The UGen may crash the
  * server if `roomSize` becomes larger than `maxRoomSize` .
  * 
  * ===Examples===
  * 
  * {{{
  * // mouse control for time and damping
  * play {
  *   val tr   = Impulse.ar(1)
  *   val in   = Decay.ar(tr, 0.25) * LFCub.ar(1200) * 0.1
  *   val damp = MouseX.kr
  *   val tail = MouseY.kr(1, 100, 1)
  *   GVerb.ar(in, roomSize = 20, revTime = tail, damping = damp, maxRoomSize = 20)
  * }
  * }}}
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb$ FreeVerb]]
  * @see [[de.sciss.synth.ugen.FreeVerb2$ FreeVerb2]]
  */
object GVerb extends ProductReader[GVerb] {
  /** @param roomSize         Size of the virtual room in meters. It must not be
    *                         greater than `maxRoomSize` . Note that quick changes in
    *                         `roomSize` may result in zipper noise and an audible
    *                         Doppler effect.
    * @param revTime          reverberation time in seconds.
    * @param damping          high frequency attenuation (1 is maximum attenuation)
    * @param inputBW          high frequency attenuation of the input signal (0 to 1)
    * @param spread           stereo spread of the reverb signal. Units?
    * @param dryLevel         amount of dry signal
    * @param earlyRefLevel    amount of early reflections
    * @param tailLevel        amount of late reverberation
    * @param maxRoomSize      maximum value that `roomSize` can take. This is used
    *                         for the early reflection delay lines and is only read at
    *                         initialization time. ''(init-time only)''
    */
  def ar(in: GE, roomSize: GE = 10.0f, revTime: GE = 3.0f, damping: GE = 0.5f, inputBW: GE = 0.5f, spread: GE = 15.0f, dryLevel: GE = 1.0f, earlyRefLevel: GE = 0.7f, tailLevel: GE = 0.5f, maxRoomSize: GE = 300.0f): GVerb = 
    new GVerb(in, roomSize, revTime, damping, inputBW, spread, dryLevel, earlyRefLevel, tailLevel, maxRoomSize)
  
  def read(in: RefMapIn, key: String, arity: Int): GVerb = {
    require (arity == 10)
    val _in             = in.readGE()
    val _roomSize       = in.readGE()
    val _revTime        = in.readGE()
    val _damping        = in.readGE()
    val _inputBW        = in.readGE()
    val _spread         = in.readGE()
    val _dryLevel       = in.readGE()
    val _earlyRefLevel  = in.readGE()
    val _tailLevel      = in.readGE()
    val _maxRoomSize    = in.readGE()
    new GVerb(_in, _roomSize, _revTime, _damping, _inputBW, _spread, _dryLevel, _earlyRefLevel, _tailLevel, _maxRoomSize)
  }
}

/** A stereophonic reverb UGen. It is based on the GVerb LADSPA effect by Juhana
  * Sadeharju.
  * 
  * '''Note:''' A CPU spike may occur when the synth is instantiated.
  * '''Warning:''' The UGen has a bug which results in loud noise if the room size
  * is increased during playback. It seems safe to start with a large room size and
  * ''decrease'' the value during playing. '''Warning:''' The UGen may crash the
  * server if `roomSize` becomes larger than `maxRoomSize` .
  * 
  * @param roomSize         Size of the virtual room in meters. It must not be
  *                         greater than `maxRoomSize` . Note that quick changes in
  *                         `roomSize` may result in zipper noise and an audible
  *                         Doppler effect.
  * @param revTime          reverberation time in seconds.
  * @param damping          high frequency attenuation (1 is maximum attenuation)
  * @param inputBW          high frequency attenuation of the input signal (0 to 1)
  * @param spread           stereo spread of the reverb signal. Units?
  * @param dryLevel         amount of dry signal
  * @param earlyRefLevel    amount of early reflections
  * @param tailLevel        amount of late reverberation
  * @param maxRoomSize      maximum value that `roomSize` can take. This is used
  *                         for the early reflection delay lines and is only read at
  *                         initialization time. ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.FreeVerb$ FreeVerb]]
  * @see [[de.sciss.synth.ugen.FreeVerb2$ FreeVerb2]]
  */
final case class GVerb(in: GE, roomSize: GE = 10.0f, revTime: GE = 3.0f, damping: GE = 0.5f, inputBW: GE = 0.5f, spread: GE = 15.0f, dryLevel: GE = 1.0f, earlyRefLevel: GE = 0.7f, tailLevel: GE = 0.5f, maxRoomSize: GE = 300.0f)
  extends UGenSource.MultiOut with AudioRated {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, roomSize.expand, revTime.expand, damping.expand, inputBW.expand, spread.expand, dryLevel.expand, earlyRefLevel.expand, tailLevel.expand, maxRoomSize.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (audio.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.MultiOut(name, audio, Vector.fill(2)(audio), _args1)
  }
  
  def left: GE = ChannelProxy(this, 0)
  
  def right: GE = ChannelProxy(this, 1)
}