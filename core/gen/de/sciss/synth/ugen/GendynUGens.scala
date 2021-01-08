// revision: 2
package de.sciss.synth
package ugen

import UGenSource._
object Gendy1 extends Reader[Gendy1] {
  def kr: Gendy1 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy1 = 
    new Gendy1(control, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum)
  
  def ar: Gendy1 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy1 = 
    new Gendy1(audio, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum)
  
  def read(in: DataInput): Gendy1 = {
    readArity(in, 11)
    val _rate     = readRate(in)
    val _ampDist  = readGE(in)
    val _durDist  = readGE(in)
    val _adParam  = readGE(in)
    val _ddParam  = readGE(in)
    val _minFreq  = readGE(in)
    val _maxFreq  = readGE(in)
    val _ampScale = readGE(in)
    val _durScale = readGE(in)
    val _initCPs  = readGE(in)
    val _kNum     = readGE(in)
    new Gendy1(_rate, _ampDist, _durDist, _adParam, _ddParam, _minFreq, _maxFreq, _ampScale, _durScale, _initCPs, _kNum)
  }
}
final case class Gendy1(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, minFreq.expand, maxFreq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object Gendy2 extends Reader[Gendy2] {
  def kr: Gendy2 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f): Gendy2 = 
    new Gendy2(control, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c)
  
  def ar: Gendy2 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f): Gendy2 = 
    new Gendy2(audio, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c)
  
  def read(in: DataInput): Gendy2 = {
    readArity(in, 13)
    val _rate     = readRate(in)
    val _ampDist  = readGE(in)
    val _durDist  = readGE(in)
    val _adParam  = readGE(in)
    val _ddParam  = readGE(in)
    val _minFreq  = readGE(in)
    val _maxFreq  = readGE(in)
    val _ampScale = readGE(in)
    val _durScale = readGE(in)
    val _initCPs  = readGE(in)
    val _kNum     = readGE(in)
    val _a        = readGE(in)
    val _c        = readGE(in)
    new Gendy2(_rate, _ampDist, _durDist, _adParam, _ddParam, _minFreq, _maxFreq, _ampScale, _durScale, _initCPs, _kNum, _a, _c)
  }
}
final case class Gendy2(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, minFreq.expand, maxFreq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand, a.expand, c.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object Gendy3 extends Reader[Gendy3] {
  def kr: Gendy3 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy3 = 
    new Gendy3(control, ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum)
  
  def ar: Gendy3 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy3 = 
    new Gendy3(audio, ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum)
  
  def read(in: DataInput): Gendy3 = {
    readArity(in, 10)
    val _rate     = readRate(in)
    val _ampDist  = readGE(in)
    val _durDist  = readGE(in)
    val _adParam  = readGE(in)
    val _ddParam  = readGE(in)
    val _freq     = readGE(in)
    val _ampScale = readGE(in)
    val _durScale = readGE(in)
    val _initCPs  = readGE(in)
    val _kNum     = readGE(in)
    new Gendy3(_rate, _ampDist, _durDist, _adParam, _ddParam, _freq, _ampScale, _durScale, _initCPs, _kNum)
  }
}
final case class Gendy3(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, freq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}