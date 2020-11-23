// revision: 2
package de.sciss.synth
package ugen

import UGenSource._
object Gendy1 {
  def kr: Gendy1 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy1 = 
    new Gendy1(control, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum)
  
  def ar: Gendy1 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy1 = 
    new Gendy1(audio, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum)
}
final case class Gendy1(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, minFreq.expand, maxFreq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object Gendy2 {
  def kr: Gendy2 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f): Gendy2 = 
    new Gendy2(control, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c)
  
  def ar: Gendy2 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f): Gendy2 = 
    new Gendy2(audio, ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c)
}
final case class Gendy2(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, minFreq: GE = 440.0f, maxFreq: GE = 660.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31f)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, minFreq.expand, maxFreq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand, a.expand, c.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}
object Gendy3 {
  def kr: Gendy3 = kr()
  
  def kr(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy3 = 
    new Gendy3(control, ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum)
  
  def ar: Gendy3 = ar()
  
  def ar(ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12): Gendy3 = 
    new Gendy3(audio, ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum)
}
final case class Gendy3(rate: Rate, ampDist: GE = 1.0f, durDist: GE = 1.0f, adParam: GE = 1.0f, ddParam: GE = 1.0f, freq: GE = 440.0f, ampScale: GE = 0.5f, durScale: GE = 0.5f, initCPs: GE = 12, kNum: GE = 12)
  extends UGenSource.SingleOut with IsIndividual {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](ampDist.expand, durDist.expand, adParam.expand, ddParam.expand, freq.expand, ampScale.expand, durScale.expand, initCPs.expand, kNum.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = UGen.SingleOut(name, rate, _args, isIndividual = true)
}