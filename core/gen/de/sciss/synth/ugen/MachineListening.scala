// revision: 4
package de.sciss.synth
package ugen

import UGenSource._

/** An autocorrelation based beat tracker UGen.
  * 
  * The underlying model assumes 4/4, but it should work on any isochronous beat
  * structure, though there are biases to 100-120 bpm; a fast 7/8 may not be tracked
  * in that sense. There are '''four''' control-rate outputs, being ticks at
  * quarter, eighth and sixteenth level from the determined beat, and the current
  * detected tempo. Note that the sixteenth note output won't necessarily make much
  * sense if the music being tracked has swing; it is provided just as a
  * convenience.
  * 
  * This beat tracker determines the beat, biased to the mid-tempo range by
  * weighting functions. It does not determine the measure level, only a tactus. It
  * is also slow reacting, using a 6 second temporal window for its autocorrelation
  * maneuvres. Don't expect human musician level predictive tracking.
  * 
  * On the other hand, it is tireless, relatively general (though obviously best at
  * transient 4/4 heavy material without much expressive tempo variation), and can
  * form the basis of computer processing that is decidedly faster than human.
  * 
  * '''Warning''': This UGen only works properly at 44.1 or 48.0 kHz.
  */
object BeatTrack extends ProductType[BeatTrack] {
  /** @param chain            the output (buffer) of an FFT UGen which transforms the
    *                         audio input to track. The expected size of FFT is 1024
    *                         for 44100 and 48000 sampling rate, and 2048 for double
    *                         those. No other sampling rates are supported.
    * @param lock             If this argument is greater than 0.5, the tracker will
    *                         lock at its current periodicity and continue from the
    *                         current phase. Whilst it updates the model's phase and
    *                         period, this is not reflected in the output until lock
    *                         goes back below 0.5. Can be control-rate modulated.
    */
  def kr(chain: GE, lock: GE = 0.0f): BeatTrack = new BeatTrack(chain, lock)
  
  final val typeId = 270
  
  def read(in: RefMapIn, key: String, arity: Int): BeatTrack = {
    require (arity == 2)
    val _chain  = in.readGE()
    val _lock   = in.readGE()
    new BeatTrack(_chain, _lock)
  }
}

/** An autocorrelation based beat tracker UGen.
  * 
  * The underlying model assumes 4/4, but it should work on any isochronous beat
  * structure, though there are biases to 100-120 bpm; a fast 7/8 may not be tracked
  * in that sense. There are '''four''' control-rate outputs, being ticks at
  * quarter, eighth and sixteenth level from the determined beat, and the current
  * detected tempo. Note that the sixteenth note output won't necessarily make much
  * sense if the music being tracked has swing; it is provided just as a
  * convenience.
  * 
  * This beat tracker determines the beat, biased to the mid-tempo range by
  * weighting functions. It does not determine the measure level, only a tactus. It
  * is also slow reacting, using a 6 second temporal window for its autocorrelation
  * maneuvres. Don't expect human musician level predictive tracking.
  * 
  * On the other hand, it is tireless, relatively general (though obviously best at
  * transient 4/4 heavy material without much expressive tempo variation), and can
  * form the basis of computer processing that is decidedly faster than human.
  * 
  * '''Warning''': This UGen only works properly at 44.1 or 48.0 kHz.
  * 
  * @param chain            the output (buffer) of an FFT UGen which transforms the
  *                         audio input to track. The expected size of FFT is 1024
  *                         for 44100 and 48000 sampling rate, and 2048 for double
  *                         those. No other sampling rates are supported.
  * @param lock             If this argument is greater than 0.5, the tracker will
  *                         lock at its current periodicity and continue from the
  *                         current phase. Whilst it updates the model's phase and
  *                         period, this is not reflected in the output until lock
  *                         goes back below 0.5. Can be control-rate modulated.
  */
final case class BeatTrack(chain: GE, lock: GE = 0.0f)
  extends UGenSource.SingleOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, lock.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}

/** A UGen for the extraction of instantaneous loudness. A perceptual loudness
  * function which outputs loudness in sones; this is a variant of an MP3 perceptual
  * model, summing excitation in ERB bands. It models simple spectral and temporal
  * masking, with equal loudness contour correction in ERB bands to obtain phons
  * (relative dB), then a phon to sone transform. The final output is typically in
  * the range of 0 to 64 sones, though higher values can occur with specific
  * synthesised stimuli.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  */
object Loudness extends ProductType[Loudness] {
  /** @param chain            the output (buffer) of an FFT UGen which transforms the
    *                         audio input to track. The FFT size should be 1024 for
    *                         44.1 and 48 kHz sampling rate, and 2048 for 88.2 and 96
    *                         kHz sampling rate.
    * @param smask            Spectral masking parameter: lower bins mask higher bin
    *                         power within ERB bands, with a power falloff (leaky
    *                         integration multiplier) of `smask` per bin. Can be
    *                         control-rate modulated.
    * @param tmask            Temporal masking parameter: the phon level let through
    *                         in an ERB band is the maximum of the new measurement,
    *                         and the previous minus `tmask` phons. Can be
    *                         control-rate modulated.
    */
  def kr(chain: GE, smask: GE = 0.25f, tmask: GE = 1.0f): Loudness = new Loudness(chain, smask, tmask)
  
  final val typeId = 271
  
  def read(in: RefMapIn, key: String, arity: Int): Loudness = {
    require (arity == 3)
    val _chain  = in.readGE()
    val _smask  = in.readGE()
    val _tmask  = in.readGE()
    new Loudness(_chain, _smask, _tmask)
  }
}

/** A UGen for the extraction of instantaneous loudness. A perceptual loudness
  * function which outputs loudness in sones; this is a variant of an MP3 perceptual
  * model, summing excitation in ERB bands. It models simple spectral and temporal
  * masking, with equal loudness contour correction in ERB bands to obtain phons
  * (relative dB), then a phon to sone transform. The final output is typically in
  * the range of 0 to 64 sones, though higher values can occur with specific
  * synthesised stimuli.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @param chain            the output (buffer) of an FFT UGen which transforms the
  *                         audio input to track. The FFT size should be 1024 for
  *                         44.1 and 48 kHz sampling rate, and 2048 for 88.2 and 96
  *                         kHz sampling rate.
  * @param smask            Spectral masking parameter: lower bins mask higher bin
  *                         power within ERB bands, with a power falloff (leaky
  *                         integration multiplier) of `smask` per bin. Can be
  *                         control-rate modulated.
  * @param tmask            Temporal masking parameter: the phon level let through
  *                         in an ERB band is the maximum of the new measurement,
  *                         and the previous minus `tmask` phons. Can be
  *                         control-rate modulated.
  */
final case class Loudness(chain: GE, smask: GE = 0.25f, tmask: GE = 1.0f)
  extends UGenSource.SingleOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, smask.expand, tmask.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}

/** A (12TET major/minor) key tracker UGen. It is based on a pitch class profile of
  * energy across FFT bins and matching this to templates for major and minor scales
  * in all transpositions. It assumes a 440 Hz concert A reference. Output is 0-11 C
  * major to B major, 12-23 C minor to B minor.
  */
object KeyTrack extends ProductType[KeyTrack] {
  /** @param chain            the output (buffer) of an FFT UGen which transforms the
    *                         audio input to track. For the FFT chain, with a standard
    *                         hop of half FFT size, the FFT size should be 4096 at
    *                         44.1 or 48 kHz and 8192 at 88.2 or 96 kHz sampling rate.
    * @param keyDecay         Number of seconds for the influence of a window on the
    *                         final key decision to decay by 40dB (to 0.01 its
    *                         original value). Can be control-rate modulated.
    * @param chromaLeak       Each frame, the chroma values are set to the previous
    *                         value multiplied by this chroma decay. 0.0 will start
    *                         each frame afresh with no memory. Can be control-rate
    *                         modulated.
    */
  def kr(chain: GE, keyDecay: GE = 2.0f, chromaLeak: GE = 0.5f): KeyTrack = 
    new KeyTrack(chain, keyDecay, chromaLeak)
  
  final val typeId = 272
  
  def read(in: RefMapIn, key: String, arity: Int): KeyTrack = {
    require (arity == 3)
    val _chain      = in.readGE()
    val _keyDecay   = in.readGE()
    val _chromaLeak = in.readGE()
    new KeyTrack(_chain, _keyDecay, _chromaLeak)
  }
}

/** A (12TET major/minor) key tracker UGen. It is based on a pitch class profile of
  * energy across FFT bins and matching this to templates for major and minor scales
  * in all transpositions. It assumes a 440 Hz concert A reference. Output is 0-11 C
  * major to B major, 12-23 C minor to B minor.
  * 
  * @param chain            the output (buffer) of an FFT UGen which transforms the
  *                         audio input to track. For the FFT chain, with a standard
  *                         hop of half FFT size, the FFT size should be 4096 at
  *                         44.1 or 48 kHz and 8192 at 88.2 or 96 kHz sampling rate.
  * @param keyDecay         Number of seconds for the influence of a window on the
  *                         final key decision to decay by 40dB (to 0.01 its
  *                         original value). Can be control-rate modulated.
  * @param chromaLeak       Each frame, the chroma values are set to the previous
  *                         value multiplied by this chroma decay. 0.0 will start
  *                         each frame afresh with no memory. Can be control-rate
  *                         modulated.
  */
final case class KeyTrack(chain: GE, keyDecay: GE = 2.0f, chromaLeak: GE = 0.5f)
  extends UGenSource.SingleOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, keyDecay.expand, chromaLeak.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}

/** A UGen for extracting mel frequency cepstral coefficients. It generates a set
  * of MFCCs; these are obtained from a band-based frequency representation (using
  * the Mel scale by default), and then a discrete cosine transform (DCT). The DCT
  * is an efficient approximation for principal components analysis, so that it
  * allows a compression, or reduction of dimensionality, of the data, in this case
  * reducing 42 band readings to a smaller set of MFCCs. A small number of features
  * (the coefficients) end up describing the spectrum. The MFCCs are commonly used
  * as timbral descriptors.
  * 
  * The output values are somewhat normalised for the range 0.0 to 1.0, but there
  * are no guarantees on exact conformance to this. Commonly, the first coefficient
  * will be the highest value. The number of output channels corresponds to the
  * number of coefficients specified. Technical note: The 0th coefficient is not
  * generated as it consists of multiplying all bands by 1 and summing
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  */
object MFCC extends ProductType[MFCC] {
  /** @param chain            the output (buffer) of an FFT UGen which transforms the
    *                         audio input to track. For the FFT chain, with a standard
    *                         hop of half FFT size, the FFT size should be 1024 at
    *                         44.1 or 48 kHz and 2048 at 88.2 or 96 kHz sampling rate.
    * @param numCoeffs        the number of coefficients, defaults to 13, maximum of
    *                         42; more efficient to use less of course! Since this
    *                         number determines the number of output channels of the
    *                         UGen, it has to be an `Int` .
    */
  def kr(chain: GE, numCoeffs: Int = 13): MFCC = new MFCC(chain, numCoeffs)
  
  final val typeId = 273
  
  def read(in: RefMapIn, key: String, arity: Int): MFCC = {
    require (arity == 2)
    val _chain      = in.readGE()
    val _numCoeffs  = in.readInt()
    new MFCC(_chain, _numCoeffs)
  }
}

/** A UGen for extracting mel frequency cepstral coefficients. It generates a set
  * of MFCCs; these are obtained from a band-based frequency representation (using
  * the Mel scale by default), and then a discrete cosine transform (DCT). The DCT
  * is an efficient approximation for principal components analysis, so that it
  * allows a compression, or reduction of dimensionality, of the data, in this case
  * reducing 42 band readings to a smaller set of MFCCs. A small number of features
  * (the coefficients) end up describing the spectrum. The MFCCs are commonly used
  * as timbral descriptors.
  * 
  * The output values are somewhat normalised for the range 0.0 to 1.0, but there
  * are no guarantees on exact conformance to this. Commonly, the first coefficient
  * will be the highest value. The number of output channels corresponds to the
  * number of coefficients specified. Technical note: The 0th coefficient is not
  * generated as it consists of multiplying all bands by 1 and summing
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @param chain            the output (buffer) of an FFT UGen which transforms the
  *                         audio input to track. For the FFT chain, with a standard
  *                         hop of half FFT size, the FFT size should be 1024 at
  *                         44.1 or 48 kHz and 2048 at 88.2 or 96 kHz sampling rate.
  * @param numCoeffs        the number of coefficients, defaults to 13, maximum of
  *                         42; more efficient to use less of course! Since this
  *                         number determines the number of output channels of the
  *                         UGen, it has to be an `Int` .
  */
final case class MFCC(chain: GE, numCoeffs: Int = 13)
  extends UGenSource.MultiOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand, numCoeffs))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, control, Vector.fill(numCoeffs)(control), _args, isIndividual = true)
}

/** An onset detecting UGen for musical audio signals. It detects the beginning of
  * notes/drumbeats/etc. Outputs a control-rate trigger signal which is 1 when an
  * onset is detected, and 0 otherwise.
  * 
  * The onset detection should work well for a general range of monophonic and
  * polyphonic audio signals. The onset detection is purely based on signal analysis
  * and does not make use of any "top-down" inferences such as tempo.
  * 
  * There are different functions available for the analysis:
  * 
  * - 0 "power" -- generally OK, good for percussive input, and also very efficient
  * - 1 "magsum" -- generally OK, good for percussive input, and also very efficient
  * - 2 "complex" -- performs generally very well, but more CPU-intensive - 3
  * "rcomplex" (default) -- performs generally very well, and slightly more
  * efficient than "complex" - 4 "phase" -- generally good, especially for tonal
  * input, medium efficiency - 5 "wphase" -- generally very good, especially for
  * tonal input, medium efficiency - 6 "mkl" -- generally very good, medium
  * efficiency, pretty different from the other methods
  * 
  * The differences aren't large, so it is recommended you stick with the default
  * "rcomplex" unless you find specific problems with it. Then maybe try "wphase".
  * The "mkl" type is a bit different from the others so maybe try that too. They
  * all have slightly different characteristics, and in tests perform at a similar
  * quality level.
  * 
  * @see [[de.sciss.synth.ugen.PV_JensenAndersen$ PV_JensenAndersen]]
  * @see [[de.sciss.synth.ugen.PV_HainsworthFoote$ PV_HainsworthFoote]]
  */
object Onsets extends ProductType[Onsets] {
  /** @param chain            the output (buffer) of an FFT UGen which transforms the
    *                         audio input to track. For the FFT chain, you should
    *                         typically use a frame size of 512 or 1024 (at 44.1 kHz
    *                         sampling rate) and 50% hop size (which is the default
    *                         setting in SC). For different sampling rates choose an
    *                         FFT size to cover a similar time-span (around 10 to 20
    *                         ms).
    * @param thresh           the detection threshold, typically between 0 and 1,
    *                         although in rare cases you may find values outside this
    *                         range useful
    * @param fun              index of a function to be used to analyse the signal.
    *                         See main paragraph for possible values (usually can be
    *                         left to default).
    * @param decay            (advanced setting) Specifies the time (in seconds) for
    *                         the normalisation to "forget" about a recent onset. If
    *                         you find too much re-triggering (e.g. as a note dies
    *                         away unevenly) then you might wish to increase this
    *                         value. Not used with "mkl".
    * @param noiseFloor       (advanced setting) This is a lower limit, connected to
    *                         the idea of how quiet the sound is expected to get
    *                         without becoming indistinguishable from noise. For some
    *                         cleanly-recorded classical music with wide dynamic
    *                         variations, it was found helpful to go down as far as
    *                         1e-6. Not used with "mkl".
    * @param minGap           (advanced setting) Specifies a minimum gap (in FFT
    *                         frames) between onset detections, a brute-force way to
    *                         prevent too many doubled detections.
    * @param medianSpan       (advanced setting) Specifies the size (in FFT frames)
    *                         of the median window used for smoothing the detection
    *                         function before triggering.
    * @param whType           (advanced setting) ?
    * @param raw              (advanced setting) ? ''(init-time only)''
    */
  def kr(chain: GE, thresh: GE = 0.5f, fun: GE = 3, decay: GE = 1.0f, noiseFloor: GE = 0.1f, minGap: GE = 10, medianSpan: GE = 11, whType: GE = 1, raw: GE = 0): Onsets = 
    new Onsets(chain, thresh, fun, decay, noiseFloor, minGap, medianSpan, whType, raw)
  
  final val typeId = 274
  
  def read(in: RefMapIn, key: String, arity: Int): Onsets = {
    require (arity == 9)
    val _chain      = in.readGE()
    val _thresh     = in.readGE()
    val _fun        = in.readGE()
    val _decay      = in.readGE()
    val _noiseFloor = in.readGE()
    val _minGap     = in.readGE()
    val _medianSpan = in.readGE()
    val _whType     = in.readGE()
    val _raw        = in.readGE()
    new Onsets(_chain, _thresh, _fun, _decay, _noiseFloor, _minGap, _medianSpan, _whType, _raw)
  }
}

/** An onset detecting UGen for musical audio signals. It detects the beginning of
  * notes/drumbeats/etc. Outputs a control-rate trigger signal which is 1 when an
  * onset is detected, and 0 otherwise.
  * 
  * The onset detection should work well for a general range of monophonic and
  * polyphonic audio signals. The onset detection is purely based on signal analysis
  * and does not make use of any "top-down" inferences such as tempo.
  * 
  * There are different functions available for the analysis:
  * 
  * - 0 "power" -- generally OK, good for percussive input, and also very efficient
  * - 1 "magsum" -- generally OK, good for percussive input, and also very efficient
  * - 2 "complex" -- performs generally very well, but more CPU-intensive - 3
  * "rcomplex" (default) -- performs generally very well, and slightly more
  * efficient than "complex" - 4 "phase" -- generally good, especially for tonal
  * input, medium efficiency - 5 "wphase" -- generally very good, especially for
  * tonal input, medium efficiency - 6 "mkl" -- generally very good, medium
  * efficiency, pretty different from the other methods
  * 
  * The differences aren't large, so it is recommended you stick with the default
  * "rcomplex" unless you find specific problems with it. Then maybe try "wphase".
  * The "mkl" type is a bit different from the others so maybe try that too. They
  * all have slightly different characteristics, and in tests perform at a similar
  * quality level.
  * 
  * @param chain            the output (buffer) of an FFT UGen which transforms the
  *                         audio input to track. For the FFT chain, you should
  *                         typically use a frame size of 512 or 1024 (at 44.1 kHz
  *                         sampling rate) and 50% hop size (which is the default
  *                         setting in SC). For different sampling rates choose an
  *                         FFT size to cover a similar time-span (around 10 to 20
  *                         ms).
  * @param thresh           the detection threshold, typically between 0 and 1,
  *                         although in rare cases you may find values outside this
  *                         range useful
  * @param fun              index of a function to be used to analyse the signal.
  *                         See main paragraph for possible values (usually can be
  *                         left to default).
  * @param decay            (advanced setting) Specifies the time (in seconds) for
  *                         the normalisation to "forget" about a recent onset. If
  *                         you find too much re-triggering (e.g. as a note dies
  *                         away unevenly) then you might wish to increase this
  *                         value. Not used with "mkl".
  * @param noiseFloor       (advanced setting) This is a lower limit, connected to
  *                         the idea of how quiet the sound is expected to get
  *                         without becoming indistinguishable from noise. For some
  *                         cleanly-recorded classical music with wide dynamic
  *                         variations, it was found helpful to go down as far as
  *                         1e-6. Not used with "mkl".
  * @param minGap           (advanced setting) Specifies a minimum gap (in FFT
  *                         frames) between onset detections, a brute-force way to
  *                         prevent too many doubled detections.
  * @param medianSpan       (advanced setting) Specifies the size (in FFT frames)
  *                         of the median window used for smoothing the detection
  *                         function before triggering.
  * @param whType           (advanced setting) ?
  * @param raw              (advanced setting) ? ''(init-time only)''
  * 
  * @see [[de.sciss.synth.ugen.PV_JensenAndersen$ PV_JensenAndersen]]
  * @see [[de.sciss.synth.ugen.PV_HainsworthFoote$ PV_HainsworthFoote]]
  */
final case class Onsets(chain: GE, thresh: GE = 0.5f, fun: GE = 3, decay: GE = 1.0f, noiseFloor: GE = 0.1f, minGap: GE = 10, medianSpan: GE = 11, whType: GE = 1, raw: GE = 0)
  extends UGenSource.SingleOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, thresh.expand, fun.expand, decay.expand, noiseFloor.expand, minGap.expand, medianSpan.expand, whType.expand, raw.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}

/** A template matching beat tracker UGen. This beat tracker is based on
  * exhaustively testing particular template patterns against feature streams; the
  * testing takes place every 0.5 seconds. The two basic templates are a straight
  * (groove=0) and a swung triplet (groove=1) pattern of 16th notes; this pattern is
  * tried out at scaling factors corresponding to the tempi from 60 to 180 bpm. This
  * is the cross-correlation method of beat tracking. A majority vote is taken on
  * the best tempo detected, but this must be confirmed by a consistency check after
  * a phase estimate. Such a consistency check helps to avoid wild fluctuating
  * estimates, but is at the expense of an additional half second delay. The latency
  * of the beat tracker with default settings is thus at least 2.5 seconds; because
  * of block-based amortisation of calculation, it is actually around 2.8 seconds
  * latency for a 2.0 second temporal window.
  * 
  * This beat tracker is designed to be flexible for user needs; you can try out
  * different window sizes, tempo weights and combinations of features. However,
  * there are no guarantees on stability and effectiveness, and you will need to
  * explore such parameters for a particular situation.
  * 
  * The UGen has '''six outputs''' corresponding to beat-tick, eighth-tick,
  * groove-tick, tempo, phase, and groove. '''Warning''': it reads from input
  * control bus instead of taking a regular control input signal as its first
  * argument!
  */
object BeatTrack2 extends ProductType[BeatTrack2] {
  /** @param bus              index of a control bus to read from. the number of
    *                         channels of that bus are expected to match the
    *                         `numChannels` argument. To track a particular audio
    *                         signal, analyse it first into `numChannels` features,
    *                         that is onset-detection-triggers, as generated by
    *                         `Onsets` , and write the trigger-output to this control
    *                         bus.
    * @param numChannels      (scalar) How many features (ie how many control bus
    *                         channels) are provided ''(init-time only)''
    * @param winSize          (scalar) Size of the temporal window desired (2.0 to
    *                         3.0 seconds models the human temporal window). You might
    *                         use longer values for stability of estimate at the
    *                         expense of reactiveness.
    * @param phaseSpacing     (scalar) Relates to how many different phases to test.
    *                         At the default of 0.02 seconds, 50 different phases
    *                         spaced by those 0.02 seconds would be tried out for
    *                         60bpm; 16 would be trialed for 180 bpm. Larger
    *                         phaseSpacing means more tests and more CPU cost.
    * @param lock             If this argument is greater than 0.5, the tracker will
    *                         lock at its current periodicity and continue from the
    *                         current phase. Whilst it updates the model's phase and
    *                         period, this is not reflected in the output until lock
    *                         goes back below 0.5. Can be control-rate modulated.
    * @param weighting        (scalar) Use (-2.5) for flat weighting of tempi, (-1.5)
    *                         for compensation weighting based on the number of events
    *                         tested (because different periods allow different
    *                         numbers of events within the temporal window). If an
    *                         integer from 0 upwards is given, this is specifying the
    *                         id of a buffer containing 120 frames which represent
    *                         individual tempo weights; tempi go from 60 to 179 bpm in
    *                         steps of one bpm, so you make sure the buffer has 120
    *                         frames.
    */
  def kr(bus: GE, numChannels: GE, winSize: GE = 2, phaseSpacing: GE = 0.02f, lock: GE = 0, weighting: GE = -2.1f): BeatTrack2 = 
    new BeatTrack2(bus, numChannels, winSize, phaseSpacing, lock, weighting)
  
  final val typeId = 275
  
  def read(in: RefMapIn, key: String, arity: Int): BeatTrack2 = {
    require (arity == 6)
    val _bus          = in.readGE()
    val _numChannels  = in.readGE()
    val _winSize      = in.readGE()
    val _phaseSpacing = in.readGE()
    val _lock         = in.readGE()
    val _weighting    = in.readGE()
    new BeatTrack2(_bus, _numChannels, _winSize, _phaseSpacing, _lock, _weighting)
  }
}

/** A template matching beat tracker UGen. This beat tracker is based on
  * exhaustively testing particular template patterns against feature streams; the
  * testing takes place every 0.5 seconds. The two basic templates are a straight
  * (groove=0) and a swung triplet (groove=1) pattern of 16th notes; this pattern is
  * tried out at scaling factors corresponding to the tempi from 60 to 180 bpm. This
  * is the cross-correlation method of beat tracking. A majority vote is taken on
  * the best tempo detected, but this must be confirmed by a consistency check after
  * a phase estimate. Such a consistency check helps to avoid wild fluctuating
  * estimates, but is at the expense of an additional half second delay. The latency
  * of the beat tracker with default settings is thus at least 2.5 seconds; because
  * of block-based amortisation of calculation, it is actually around 2.8 seconds
  * latency for a 2.0 second temporal window.
  * 
  * This beat tracker is designed to be flexible for user needs; you can try out
  * different window sizes, tempo weights and combinations of features. However,
  * there are no guarantees on stability and effectiveness, and you will need to
  * explore such parameters for a particular situation.
  * 
  * The UGen has '''six outputs''' corresponding to beat-tick, eighth-tick,
  * groove-tick, tempo, phase, and groove. '''Warning''': it reads from input
  * control bus instead of taking a regular control input signal as its first
  * argument!
  * 
  * @param bus              index of a control bus to read from. the number of
  *                         channels of that bus are expected to match the
  *                         `numChannels` argument. To track a particular audio
  *                         signal, analyse it first into `numChannels` features,
  *                         that is onset-detection-triggers, as generated by
  *                         `Onsets` , and write the trigger-output to this control
  *                         bus.
  * @param numChannels      (scalar) How many features (ie how many control bus
  *                         channels) are provided ''(init-time only)''
  * @param winSize          (scalar) Size of the temporal window desired (2.0 to
  *                         3.0 seconds models the human temporal window). You might
  *                         use longer values for stability of estimate at the
  *                         expense of reactiveness.
  * @param phaseSpacing     (scalar) Relates to how many different phases to test.
  *                         At the default of 0.02 seconds, 50 different phases
  *                         spaced by those 0.02 seconds would be tried out for
  *                         60bpm; 16 would be trialed for 180 bpm. Larger
  *                         phaseSpacing means more tests and more CPU cost.
  * @param lock             If this argument is greater than 0.5, the tracker will
  *                         lock at its current periodicity and continue from the
  *                         current phase. Whilst it updates the model's phase and
  *                         period, this is not reflected in the output until lock
  *                         goes back below 0.5. Can be control-rate modulated.
  * @param weighting        (scalar) Use (-2.5) for flat weighting of tempi, (-1.5)
  *                         for compensation weighting based on the number of events
  *                         tested (because different periods allow different
  *                         numbers of events within the temporal window). If an
  *                         integer from 0 upwards is given, this is specifying the
  *                         id of a buffer containing 120 frames which represent
  *                         individual tempo weights; tempi go from 60 to 179 bpm in
  *                         steps of one bpm, so you make sure the buffer has 120
  *                         frames.
  */
final case class BeatTrack2(bus: GE, numChannels: GE, winSize: GE = 2, phaseSpacing: GE = 0.02f, lock: GE = 0, weighting: GE = -2.1f)
  extends UGenSource.MultiOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](bus.expand, numChannels.expand, winSize.expand, phaseSpacing.expand, lock.expand, weighting.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.MultiOut(name, control, Vector.fill(6)(control), _args, isIndividual = true)
  
  def beatTick: GE = ChannelProxy(this, 0)
  
  def eighthTick: GE = ChannelProxy(this, 1)
  
  def grooveTick: GE = ChannelProxy(this, 2)
  
  def tempo: GE = ChannelProxy(this, 3)
  
  def phase: GE = ChannelProxy(this, 4)
  
  def groove: GE = ChannelProxy(this, 5)
}

/** A UGen to measure spectral flatness. Given an FFT chain this calculates the
  * Spectral Flatness measure, defined as a power spectrum's geometric mean divided
  * by its arithmetic mean. This gives a measure which ranges from approx 0 for a
  * pure sinusoid, to approx 1 for white noise.
  * 
  * The measure is calculated linearly. For some applications you may wish to
  * convert the value to a decibel scale. '''Note''' that this UGen may output NaN
  * when the input is zero (probably due to division by zero). In that case,
  * `CheckBadValues` can be used to prevent further problems.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @see [[de.sciss.synth.ugen.CheckBadValues$ CheckBadValues]]
  */
object SpecFlatness extends ProductType[SpecFlatness] {
  /** @param chain            the fft signal (buffer) to analyze
    */
  def kr(chain: GE): SpecFlatness = new SpecFlatness(chain)
  
  final val typeId = 276
  
  def read(in: RefMapIn, key: String, arity: Int): SpecFlatness = {
    require (arity == 1)
    val _chain = in.readGE()
    new SpecFlatness(_chain)
  }
}

/** A UGen to measure spectral flatness. Given an FFT chain this calculates the
  * Spectral Flatness measure, defined as a power spectrum's geometric mean divided
  * by its arithmetic mean. This gives a measure which ranges from approx 0 for a
  * pure sinusoid, to approx 1 for white noise.
  * 
  * The measure is calculated linearly. For some applications you may wish to
  * convert the value to a decibel scale. '''Note''' that this UGen may output NaN
  * when the input is zero (probably due to division by zero). In that case,
  * `CheckBadValues` can be used to prevent further problems.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @param chain            the fft signal (buffer) to analyze
  * 
  * @see [[de.sciss.synth.ugen.CheckBadValues$ CheckBadValues]]
  */
final case class SpecFlatness(chain: GE) extends UGenSource.SingleOut with ControlRated with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}

/** A UGen to find the percentile of a signal's magnitude spectrum. Given an FFT
  * chain this calculates the cumulative distribution of the frequency spectrum, and
  * outputs the frequency value which corresponds to the desired percentile. For
  * example, to find the frequency at which 90% of the spectral energy lies below
  * that frequency, you want the 90-percentile, which means the value of `percent`
  * should be 0.9. The 90-percentile or 95-percentile is often used as a measure of
  * spectral roll-off.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  */
object SpecPcile extends ProductType[SpecPcile] {
  /** @param chain            the fft signal (buffer) to analyze
    * @param percent          the percentage between 0.0 (0%) and 1.0 (100%)
    * @param interp           specifies whether interpolation should be used to try
    *                         and make the percentile frequency estimate more
    *                         accurate, at the cost of a little higher CPU usage. Set
    *                         it to 1 to enable this.
    */
  def kr(chain: GE, percent: GE = 0.5f, interp: GE = 0): SpecPcile = 
    new SpecPcile(chain, percent, interp)
  
  final val typeId = 277
  
  def read(in: RefMapIn, key: String, arity: Int): SpecPcile = {
    require (arity == 3)
    val _chain    = in.readGE()
    val _percent  = in.readGE()
    val _interp   = in.readGE()
    new SpecPcile(_chain, _percent, _interp)
  }
}

/** A UGen to find the percentile of a signal's magnitude spectrum. Given an FFT
  * chain this calculates the cumulative distribution of the frequency spectrum, and
  * outputs the frequency value which corresponds to the desired percentile. For
  * example, to find the frequency at which 90% of the spectral energy lies below
  * that frequency, you want the 90-percentile, which means the value of `percent`
  * should be 0.9. The 90-percentile or 95-percentile is often used as a measure of
  * spectral roll-off.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @param chain            the fft signal (buffer) to analyze
  * @param percent          the percentage between 0.0 (0%) and 1.0 (100%)
  * @param interp           specifies whether interpolation should be used to try
  *                         and make the percentile frequency estimate more
  *                         accurate, at the cost of a little higher CPU usage. Set
  *                         it to 1 to enable this.
  */
final case class SpecPcile(chain: GE, percent: GE = 0.5f, interp: GE = 0)
  extends UGenSource.SingleOut with ControlRated with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](chain.expand, percent.expand, interp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}

/** A UGen to measure the spectral centroid. Given an FFT chain, this measures the
  * spectral centroid, which is the weighted mean frequency, or the "centre of mass"
  * of the spectrum. (DC is ignored.) This can be a useful indicator of the
  * perceptual brightness of a signal.
  * 
  * Note that the output frequency is pretty close to the correct value when
  * feeding in a sine signal, but the estimate is usually too high when using for
  * example filtered noise. In that case, you will get better results using
  * `SpecPcile` at 50%.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @see [[de.sciss.synth.ugen.SpecPcile$ SpecPcile]]
  */
object SpecCentroid extends ProductType[SpecCentroid] {
  /** @param chain            the fft signal (buffer) to analyze
    */
  def kr(chain: GE): SpecCentroid = new SpecCentroid(chain)
  
  final val typeId = 278
  
  def read(in: RefMapIn, key: String, arity: Int): SpecCentroid = {
    require (arity == 1)
    val _chain = in.readGE()
    new SpecCentroid(_chain)
  }
}

/** A UGen to measure the spectral centroid. Given an FFT chain, this measures the
  * spectral centroid, which is the weighted mean frequency, or the "centre of mass"
  * of the spectrum. (DC is ignored.) This can be a useful indicator of the
  * perceptual brightness of a signal.
  * 
  * Note that the output frequency is pretty close to the correct value when
  * feeding in a sine signal, but the estimate is usually too high when using for
  * example filtered noise. In that case, you will get better results using
  * `SpecPcile` at 50%.
  * 
  * Note that despite running at control-rate, the output remains constant for each
  * FFT frame. E.g. with an FFT size of 1024 and 50% overlap, a new measure is
  * generated every 512 audio frames, or (at control block size 64) every 8 control
  * blocks.
  * 
  * @param chain            the fft signal (buffer) to analyze
  * 
  * @see [[de.sciss.synth.ugen.SpecPcile$ SpecPcile]]
  */
final case class SpecCentroid(chain: GE) extends UGenSource.SingleOut with ControlRated with IsIndividual {
  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](chain.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = 
    UGen.SingleOut(name, control, _args, isIndividual = true)
}