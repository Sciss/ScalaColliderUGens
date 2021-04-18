// revision: 3
package de.sciss.synth
package ugen

import UGenSource._
object Gendy1 extends ProductType[Gendy1] {
  def kr: Gendy1 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy1 = 
    new Gendy1(control, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum)
  
  def ar: Gendy1 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy1 = 
    new Gendy1(audio, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum)
  
  final val typeId = 208
  
  def read(in: RefMapIn, key: String, arity: Int): Gendy1 = {
    require (arity == 11)
    val _rate     = in.readRate()
    val _ampDist  = in.readGE()
    val _durDist  = in.readGE()
    val _adParam  = in.readGE()
    val _ddParam  = in.readGE()
    val _minFreq  = in.readGE()
    val _maxFreq  = in.readGE()
    val _ampScale = in.readGE()
    val _durScale = in.readGE()
    val _initCPs  = in.readGE()
    val _kNum     = in.readGE()
    new Gendy1(_rate, _ampDist, _durDist, _adParam, _ddParam, _minFreq, _maxFreq, _ampScale, _durScale, _initCPs, _kNum)
  }
}
final case class Gendy1(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, minFreq.expand, maxFreq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object Gendy2 extends ProductType[Gendy2] {
  def kr: Gendy2 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f): Gendy2 = 
    new Gendy2(control, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c)
  
  def ar: Gendy2 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f): Gendy2 = 
    new Gendy2(audio, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c)
  
  final val typeId = 209
  
  def read(in: RefMapIn, key: String, arity: Int): Gendy2 = {
    require (arity == 13)
    val _rate     = in.readRate()
    val _ampDist  = in.readGE()
    val _durDist  = in.readGE()
    val _adParam  = in.readGE()
    val _ddParam  = in.readGE()
    val _minFreq  = in.readGE()
    val _maxFreq  = in.readGE()
    val _ampScale = in.readGE()
    val _durScale = in.readGE()
    val _initCPs  = in.readGE()
    val _kNum     = in.readGE()
    val _a        = in.readGE()
    val _c        = in.readGE()
    new Gendy2(_rate, _ampDist, _durDist, _adParam, _ddParam, _minFreq, _maxFreq, _ampScale, _durScale, _initCPs, _kNum, _a, _c)
  }
}
final case class Gendy2(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, minFreq.expand, maxFreq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand, a.expand, c.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object Gendy3 extends ProductType[Gendy3] {
  def kr: Gendy3 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy3 = 
    new Gendy3(control, ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum)
  
  def ar: Gendy3 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy3 = 
    new Gendy3(audio, ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum)
  
  final val typeId = 210
  
  def read(in: RefMapIn, key: String, arity: Int): Gendy3 = {
    require (arity == 10)
    val _rate     = in.readRate()
    val _ampDist  = in.readGE()
    val _durDist  = in.readGE()
    val _adParam  = in.readGE()
    val _ddParam  = in.readGE()
    val _freq     = in.readGE()
    val _ampScale = in.readGE()
    val _durScale = in.readGE()
    val _initCPs  = in.readGE()
    val _kNum     = in.readGE()
    new Gendy3(_rate, _ampDist, _durDist, _adParam, _ddParam, _freq, _ampScale, _durScale, _initCPs, _kNum)
  }
}
final case class Gendy3(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, freq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}