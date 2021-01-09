// revision: 2
package de.sciss.synth
package ugen

import UGenSource._
object Spring extends ProductReader[Spring] {
  def kr(in: GE, spring: GE = 1.0f, damp: GE = 0.0f): Spring = new Spring(control, in, spring, damp)
  
  def ar(in: GE, spring: GE = 1.0f, damp: GE = 0.0f): Spring = new Spring(audio, in, spring, damp)
  
  def read(in: RefMapIn, arity: Int): Spring = {
    require (arity == 4)
    val _rate   = in.readMaybeRate()
    val _in     = in.readGE()
    val _spring = in.readGE()
    val _damp   = in.readGE()
    new Spring(_rate, _in, _spring, _damp)
  }
}
final case class Spring(rate: MaybeRate, in: GE, spring: GE = 1.0f, damp: GE = 0.0f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, spring.expand, damp.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}
object Ball extends ProductReader[Ball] {
  def kr(in: GE, g: GE = 1.0f, damp: GE = 0.0f, friction: GE = 0.01f): Ball = 
    new Ball(control, in, g, damp, friction)
  
  def ar(in: GE, g: GE = 1.0f, damp: GE = 0.0f, friction: GE = 0.01f): Ball = 
    new Ball(audio, in, g, damp, friction)
  
  def read(in: RefMapIn, arity: Int): Ball = {
    require (arity == 5)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _g        = in.readGE()
    val _damp     = in.readGE()
    val _friction = in.readGE()
    new Ball(_rate, _in, _g, _damp, _friction)
  }
}
final case class Ball(rate: MaybeRate, in: GE, g: GE = 1.0f, damp: GE = 0.0f, friction: GE = 0.01f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, g.expand, damp.expand, friction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}
object TBall extends ProductReader[TBall] {
  def kr(in: GE, g: GE = 10.0f, damp: GE = 0.0f, friction: GE = 0.01f): TBall = 
    new TBall(control, in, g, damp, friction)
  
  def ar(in: GE, g: GE = 10.0f, damp: GE = 0.0f, friction: GE = 0.01f): TBall = 
    new TBall(audio, in, g, damp, friction)
  
  def read(in: RefMapIn, arity: Int): TBall = {
    require (arity == 5)
    val _rate     = in.readMaybeRate()
    val _in       = in.readGE()
    val _g        = in.readGE()
    val _damp     = in.readGE()
    val _friction = in.readGE()
    new TBall(_rate, _in, _g, _damp, _friction)
  }
}
final case class TBall(rate: MaybeRate, in: GE, g: GE = 10.0f, damp: GE = 0.0f, friction: GE = 0.01f)
  extends UGenSource.SingleOut {

  protected def makeUGens: UGenInLike = 
    unwrap(this, Vector[UGenInLike](in.expand, g.expand, damp.expand, friction.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _rate = rate.getOrElse(_args(0).rate)
    val _args1 = matchRate(_args, 0, _rate)
    UGen.SingleOut(name, _rate, _args1)
  }
}