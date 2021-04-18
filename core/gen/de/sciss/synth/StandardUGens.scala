package de.sciss.synth

import UGenSource._
import ugen._

object StandardUGens {
  private lazy val _init: Unit = UGenSource.addProductTypes(map)

  def init(): Unit = _init

  type V = ProductType[Product]

  private def map = Map[String, V](
    ("A2K", A2K),
    ("APF", APF),
    ("AllpassC", AllpassC),
    ("AllpassL", AllpassL),
    ("AllpassN", AllpassN),
    ("AmpComp", AmpComp),
    ("AmpCompA", AmpCompA),
    ("Amplitude", Amplitude),
    ("AudioControl", AudioControl),
    ("AudioControlProxy", AudioControlProxy),
    ("BAllPass", BAllPass),
    ("BBandPass", BBandPass),
    ("BBandStop", BBandStop),
    ("BHiPass", BHiPass),
    ("BHiShelf", BHiShelf),
    ("BLowPass", BLowPass),
    ("BLowShelf", BLowShelf),
    ("BPF", BPF),
    ("BPZ2", BPZ2),
    ("BPeakEQ", BPeakEQ),
    ("BRF", BRF),
    ("BRZ2", BRZ2),
    ("Balance2", Balance2),
    ("Ball", Ball),
    ("BeatTrack", BeatTrack),
    ("BeatTrack2", BeatTrack2),
    ("BiPanB2", BiPanB2),
    ("BinaryOpUGen", BinaryOpUGen),
    ("BinaryOpUGen$Op", BinaryOpUGen.Op),
    ("Blip", Blip),
    ("BrownNoise", BrownNoise),
    ("BufAllpassC", BufAllpassC),
    ("BufAllpassL", BufAllpassL),
    ("BufAllpassN", BufAllpassN),
    ("BufChannels", BufChannels),
    ("BufCombC", BufCombC),
    ("BufCombL", BufCombL),
    ("BufCombN", BufCombN),
    ("BufDelayC", BufDelayC),
    ("BufDelayL", BufDelayL),
    ("BufDelayN", BufDelayN),
    ("BufDur", BufDur),
    ("BufFrames", BufFrames),
    ("BufRateScale", BufRateScale),
    ("BufRd", BufRd),
    ("BufSampleRate", BufSampleRate),
    ("BufSamples", BufSamples),
    ("BufWr", BufWr),
    ("COsc", COsc),
    ("ChannelIndices", ChannelIndices),
    ("ChannelProxy", ChannelProxy),
    ("ChannelRangeProxy", ChannelRangeProxy),
    ("CheckBadValues", CheckBadValues),
    ("ClearBuf", ClearBuf),
    ("Clip", Clip),
    ("ClipNoise", ClipNoise),
    ("CoinGate", CoinGate),
    ("CombC", CombC),
    ("CombL", CombL),
    ("CombN", CombN),
    ("Compander", Compander),
    ("Control", Control),
    ("ControlDur", ControlDur),
    ("ControlProxy", ControlProxy),
    ("ControlRate", ControlRate),
    ("Convolution", Convolution),
    ("Convolution2", Convolution2),
    ("Convolution2L", Convolution2L),
    ("Convolution3", Convolution3),
    ("Crackle", Crackle),
    ("CuspL", CuspL),
    ("CuspN", CuspN),
    ("DC", DC),
    ("Dbrown", Dbrown),
    ("Dbufrd", Dbufrd),
    ("Dbufwr", Dbufwr),
    ("Dconst", Dconst),
    ("Decay", Decay),
    ("Decay2", Decay2),
    ("DecodeB2", DecodeB2),
    ("DegreeToKey", DegreeToKey),
    ("DelTapRd", DelTapRd),
    ("DelTapWr", DelTapWr),
    ("Delay1", Delay1),
    ("Delay2", Delay2),
    ("DelayC", DelayC),
    ("DelayL", DelayL),
    ("DelayN", DelayN),
    ("Demand", Demand),
    ("DemandEnvGen", DemandEnvGen),
    ("DetectIndex", DetectIndex),
    ("DetectSilence", DetectSilence),
    ("Dgeom", Dgeom),
    ("Dibrown", Dibrown),
    ("DiskIn", DiskIn),
    ("DiskOut", DiskOut),
    ("Diwhite", Diwhite),
    ("Donce", Donce),
    ("Done", Done),
    ("Dpoll", Dpoll),
    ("Drand", Drand),
    ("Dreset", Dreset),
    ("Dseq", Dseq),
    ("Dser", Dser),
    ("Dseries", Dseries),
    ("Dshuf", Dshuf),
    ("Dstutter", Dstutter),
    ("Dswitch", Dswitch),
    ("Dswitch1", Dswitch1),
    ("Dust", Dust),
    ("Dust2", Dust2),
    ("Duty", Duty),
    ("Dwhite", Dwhite),
    ("Dxrand", Dxrand),
    ("Env", Env),
    ("Env$Curve$Apply", Env.Curve),
    ("Env$Curve$Const", Env.Curve),
    ("Env$Segment", Env.Segment),
    ("EnvGen", EnvGen),
    ("ExpRand", ExpRand),
    ("FBSineC", FBSineC),
    ("FBSineL", FBSineL),
    ("FBSineN", FBSineN),
    ("FFT", FFT),
    ("FFTTrigger", FFTTrigger),
    ("FOS", FOS),
    ("FSinOsc", FSinOsc),
    ("Flatten", Flatten),
    ("Fold", Fold),
    ("FoldIndex", FoldIndex),
    ("Formant", Formant),
    ("Formlet", Formlet),
    ("Free", Free),
    ("FreeSelf", FreeSelf),
    ("FreeSelfWhenDone", FreeSelfWhenDone),
    ("FreeVerb", FreeVerb),
    ("FreeVerb2", FreeVerb2),
    ("FreqShift", FreqShift),
    ("GESeq", GESeq),
    ("GVerb", GVerb),
    ("Gate", Gate),
    ("GbmanL", GbmanL),
    ("GbmanN", GbmanN),
    ("Gendy1", Gendy1),
    ("Gendy2", Gendy2),
    ("Gendy3", Gendy3),
    ("GrainBuf", GrainBuf),
    ("GrainFM", GrainFM),
    ("GrainIn", GrainIn),
    ("GrainSin", GrainSin),
    ("GrayNoise", GrayNoise),
    ("HPF", HPF),
    ("HPZ1", HPZ1),
    ("HPZ2", HPZ2),
    ("Hasher", Hasher),
    ("HenonC", HenonC),
    ("HenonL", HenonL),
    ("HenonN", HenonN),
    ("Hilbert", Hilbert),
    ("IEnv", IEnv),
    ("IEnvGen", IEnvGen),
    ("IFFT", IFFT),
    ("IRand", IRand),
    ("Impulse", Impulse),
    ("In", In),
    ("InFeedback", InFeedback),
    ("InRange", InRange),
    ("InRect", InRect),
    ("InTrig", InTrig),
    ("Index", Index),
    ("IndexInBetween", IndexInBetween),
    ("IndexL", IndexL),
    ("Integrator", Integrator),
    ("K2A", K2A),
    ("KeyState", KeyState),
    ("KeyTrack", KeyTrack),
    ("Klang", Klang),
    ("KlangSpec", KlangSpec),
    ("KlangSpec$Seq", KlangSpec.Seq),
    ("Klank", Klank),
    ("LFClipNoise", LFClipNoise),
    ("LFCub", LFCub),
    ("LFDClipNoise", LFDClipNoise),
    ("LFDNoise0", LFDNoise0),
    ("LFDNoise1", LFDNoise1),
    ("LFDNoise3", LFDNoise3),
    ("LFGauss", LFGauss),
    ("LFNoise0", LFNoise0),
    ("LFNoise1", LFNoise1),
    ("LFNoise2", LFNoise2),
    ("LFPar", LFPar),
    ("LFPulse", LFPulse),
    ("LFSaw", LFSaw),
    ("LFTri", LFTri),
    ("LPF", LPF),
    ("LPZ1", LPZ1),
    ("LPZ2", LPZ2),
    ("Lag", Lag),
    ("Lag2", Lag2),
    ("Lag2UD", Lag2UD),
    ("Lag3", Lag3),
    ("Lag3UD", Lag3UD),
    ("LagIn", LagIn),
    ("LagUD", LagUD),
    ("LastValue", LastValue),
    ("Latch", Latch),
    ("LatoocarfianC", LatoocarfianC),
    ("LatoocarfianL", LatoocarfianL),
    ("LatoocarfianN", LatoocarfianN),
    ("LeakDC", LeakDC),
    ("LeastChange", LeastChange),
    ("Limiter", Limiter),
    ("LinCongC", LinCongC),
    ("LinCongL", LinCongL),
    ("LinCongN", LinCongN),
    ("LinExp", LinExp),
    ("LinLin", LinLin),
    ("LinPan2", LinPan2),
    ("LinRand", LinRand),
    ("LinXFade2", LinXFade2),
    ("Line", Line),
    ("Linen", Linen),
    ("LocalBuf", LocalBuf),
    ("LocalIn", LocalIn),
    ("LocalOut", LocalOut),
    ("Logistic", Logistic),
    ("LorenzL", LorenzL),
    ("Loudness", Loudness),
    ("MFCC", MFCC),
    ("MantissaMask", MantissaMask),
    ("MaxLocalBufs", MaxLocalBufs),
    ("Median", Median),
    ("MidEQ", MidEQ),
    ("Mix", Mix),
    ("Mix$Mono", Mix.Mono),
    ("MoogFF", MoogFF),
    ("MostChange", MostChange),
    ("MouseButton", MouseButton),
    ("MouseX", MouseX),
    ("MouseY", MouseY),
    ("MulAdd", MulAdd),
    ("NRand", NRand),
    ("NodeID", NodeID),
    ("Normalizer", Normalizer),
    ("NumAudioBuses", NumAudioBuses),
    ("NumBuffers", NumBuffers),
    ("NumChannels", NumChannels),
    ("NumControlBuses", NumControlBuses),
    ("NumInputBuses", NumInputBuses),
    ("NumOutputBuses", NumOutputBuses),
    ("NumRunningSynths", NumRunningSynths),
    ("Nyquist", Nyquist),
    ("OffsetOut", OffsetOut),
    ("OnePole", OnePole),
    ("OneZero", OneZero),
    ("Onsets", Onsets),
    ("Osc", Osc),
    ("OscN", OscN),
    ("Out", Out),
    ("PV_Add", PV_Add),
    ("PV_BinScramble", PV_BinScramble),
    ("PV_BinShift", PV_BinShift),
    ("PV_BinWipe", PV_BinWipe),
    ("PV_BrickWall", PV_BrickWall),
    ("PV_ConformalMap", PV_ConformalMap),
    ("PV_Conj", PV_Conj),
    ("PV_Copy", PV_Copy),
    ("PV_CopyPhase", PV_CopyPhase),
    ("PV_Diffuser", PV_Diffuser),
    ("PV_Div", PV_Div),
    ("PV_HainsworthFoote", PV_HainsworthFoote),
    ("PV_JensenAndersen", PV_JensenAndersen),
    ("PV_LocalMax", PV_LocalMax),
    ("PV_MagAbove", PV_MagAbove),
    ("PV_MagBelow", PV_MagBelow),
    ("PV_MagClip", PV_MagClip),
    ("PV_MagDiv", PV_MagDiv),
    ("PV_MagFreeze", PV_MagFreeze),
    ("PV_MagMul", PV_MagMul),
    ("PV_MagNoise", PV_MagNoise),
    ("PV_MagShift", PV_MagShift),
    ("PV_MagSmear", PV_MagSmear),
    ("PV_MagSquared", PV_MagSquared),
    ("PV_Max", PV_Max),
    ("PV_Min", PV_Min),
    ("PV_Mul", PV_Mul),
    ("PV_PhaseShift", PV_PhaseShift),
    ("PV_PhaseShift270", PV_PhaseShift270),
    ("PV_PhaseShift90", PV_PhaseShift90),
    ("PV_RandComb", PV_RandComb),
    ("PV_RandWipe", PV_RandWipe),
    ("PV_RectComb", PV_RectComb),
    ("PV_RectComb2", PV_RectComb2),
    ("PackFFT", PackFFT),
    ("Pad", Pad),
    ("Pan2", Pan2),
    ("Pan4", Pan4),
    ("PanAz", PanAz),
    ("PanB", PanB),
    ("PanB2", PanB2),
    ("PartConv", PartConv),
    ("Pause", Pause),
    ("PauseSelf", PauseSelf),
    ("PauseSelfWhenDone", PauseSelfWhenDone),
    ("Peak", Peak),
    ("PeakFollower", PeakFollower),
    ("Phasor", Phasor),
    ("PhysicalIn", PhysicalIn),
    ("PhysicalOut", PhysicalOut),
    ("PinkNoise", PinkNoise),
    ("Pitch", Pitch),
    ("PitchShift", PitchShift),
    ("PlayBuf", PlayBuf),
    ("Pluck", Pluck),
    ("Poll", Poll),
    ("Pulse", Pulse),
    ("PulseCount", PulseCount),
    ("PulseDivider", PulseDivider),
    ("QuadC", QuadC),
    ("QuadL", QuadL),
    ("QuadN", QuadN),
    ("RHPF", RHPF),
    ("RLPF", RLPF),
    ("RadiansPerSample", RadiansPerSample),
    ("Ramp", Ramp),
    ("Rand", Rand),
    ("RandID", RandID),
    ("RandSeed", RandSeed),
    ("RecordBuf", RecordBuf),
    ("Reduce", Reduce),
    ("RepeatChannels", RepeatChannels),
    ("ReplaceOut", ReplaceOut),
    ("Resonz", Resonz),
    ("Ringz", Ringz),
    ("Rotate2", Rotate2),
    ("RunningMax", RunningMax),
    ("RunningMin", RunningMin),
    ("RunningSum", RunningSum),
    ("SOS", SOS),
    ("SampleDur", SampleDur),
    ("SampleRate", SampleRate),
    ("Saw", Saw),
    ("Schmidt", Schmidt),
    ("ScopeOut", ScopeOut),
    ("ScopeOut2", ScopeOut2),
    ("Select", Select),
    ("SendReply", SendReply),
    ("SendTrig", SendTrig),
    ("SetBuf", SetBuf),
    ("SetResetFF", SetResetFF),
    ("Shaper", Shaper),
    ("Silent", Silent),
    ("SinOsc", SinOsc),
    ("SinOscFB", SinOscFB),
    ("Slew", Slew),
    ("Slope", Slope),
    ("SpecCentroid", SpecCentroid),
    ("SpecFlatness", SpecFlatness),
    ("SpecPcile", SpecPcile),
    ("SplayAz", SplayAz),
    ("Spring", Spring),
    ("StandardL", StandardL),
    ("StandardN", StandardN),
    ("Stepper", Stepper),
    ("StereoConvolution2L", StereoConvolution2L),
    ("SubsampleOffset", SubsampleOffset),
    ("Sum3", Sum3),
    ("Sum4", Sum4),
    ("Sweep", Sweep),
    ("SyncSaw", SyncSaw),
    ("T2A", T2A),
    ("T2K", T2K),
    ("TBall", TBall),
    ("TDelay", TDelay),
    ("TDuty", TDuty),
    ("TExpRand", TExpRand),
    ("TGrains", TGrains),
    ("TIRand", TIRand),
    ("TRand", TRand),
    ("TWindex", TWindex),
    ("Timer", Timer),
    ("ToggleFF", ToggleFF),
    ("Trig", Trig),
    ("Trig1", Trig1),
    ("TrigControl", TrigControl),
    ("TrigControlProxy", TrigControlProxy),
    ("TwoPole", TwoPole),
    ("TwoZero", TwoZero),
    ("UGenInSeq", UGenInSeq),
    ("UnaryOpUGen", UnaryOpUGen),
    ("UnaryOpUGen$Op", UnaryOpUGen.Op),
    ("Unpack1FFT", Unpack1FFT),
    ("VDiskIn", VDiskIn),
    ("VOsc", VOsc),
    ("VOsc3", VOsc3),
    ("VarSaw", VarSaw),
    ("Vibrato", Vibrato),
    ("Warp1", Warp1),
    ("WhiteNoise", WhiteNoise),
    ("Wrap", Wrap),
    ("WrapIndex", WrapIndex),
    ("WrapOut", WrapOut),
    ("XFade2", XFade2),
    ("XLine", XLine),
    ("XOut", XOut),
    ("ZeroCrossing", ZeroCrossing),
    ("Zip", Zip),
    ("de.sciss.synth.Curve$cubed", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$exponential", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$linear", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$parametric", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$sine", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$squared", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$step", de.sciss.synth.Curve),
    ("de.sciss.synth.Curve$welch", de.sciss.synth.Curve),
  )
}
