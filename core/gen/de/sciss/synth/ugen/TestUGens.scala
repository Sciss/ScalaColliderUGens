// revision: 2
package de.sciss.synth
package ugen

import UGenSource._

/** A UGen to test for infinity, not-a-number (NaN), and denormal numbers. Its
  * output is as follows: 0 = a normal float, 1 = NaN, 2 = infinity, and 3 = a
  * denormal. According to the post settings it will print the information to the
  * console along with a given identifier.
  */
object CheckBadValues extends ProductReader[CheckBadValues] {
  /** @param in               the signal to be tested
    * @param id               an identifier showing up with the values in the console
    * @param post             One of three post modes: 0 = no posting; 1 = post a
    *                         line for every bad value; 2 = post a line only when the
    *                         floating-point classification changes (e.g., normal ->
    *                         NaN and vice versa)
    */
  def ir(in: GE, id: GE = 0, post: GE = 2): CheckBadValues = new CheckBadValues(scalar, in, id, post)
  
  /** @param in               the signal to be tested
    * @param id               an identifier showing up with the values in the console
    * @param post             One of three post modes: 0 = no posting; 1 = post a
    *                         line for every bad value; 2 = post a line only when the
    *                         floating-point classification changes (e.g., normal ->
    *                         NaN and vice versa)
    */
  def kr(in: GE, id: GE = 0, post: GE = 2): CheckBadValues = new CheckBadValues(control, in, id, post)
  
  /** @param in               the signal to be tested
    * @param id               an identifier showing up with the values in the console
    * @param post             One of three post modes: 0 = no posting; 1 = post a
    *                         line for every bad value; 2 = post a line only when the
    *                         floating-point classification changes (e.g., normal ->
    *                         NaN and vice versa)
    */
  def ar(in: GE, id: GE = 0, post: GE = 2): CheckBadValues = new CheckBadValues(audio, in, id, post)
  
  def read(in: RefMapIn, key: String, arity: Int): CheckBadValues = {
    require (arity == 4)
    val _rate = in.readRate()
    val _in   = in.readGE()
    val _id   = in.readGE()
    val _post = in.readGE()
    new CheckBadValues(_rate, _in, _id, _post)
  }
}

/** A UGen to test for infinity, not-a-number (NaN), and denormal numbers. Its
  * output is as follows: 0 = a normal float, 1 = NaN, 2 = infinity, and 3 = a
  * denormal. According to the post settings it will print the information to the
  * console along with a given identifier.
  * 
  * @param in               the signal to be tested
  * @param id               an identifier showing up with the values in the console
  * @param post             One of three post modes: 0 = no posting; 1 = post a
  *                         line for every bad value; 2 = post a line only when the
  *                         floating-point classification changes (e.g., normal ->
  *                         NaN and vice versa)
  */
final case class CheckBadValues(rate: Rate, in: GE, id: GE = 0, post: GE = 2)
  extends UGenSource.SingleOut with HasSideEffect {

  protected def makeUGens: UGenInLike = unwrap(this, Vector[UGenInLike](in.expand, id.expand, post.expand))
  
  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    val _args1 = if (rate.==(audio)) matchRate(_args, 0, audio) else _args
    UGen.SingleOut(name, rate, _args1, hasSideEffect = true)
  }
}