/*
 *  LocalBuf.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import UGenSource._

// XXX TODO - SetBuf, ClearBuf

/** A UGen that allocates a buffer local to the synth.
  * This is convenient for example when using an `FFT` chain.
  *
  * @see [[de.sciss.synth.ugen.FFT]]
  */
object LocalBuf
/** A UGen that allocates a buffer local to the synth.
  * This is convenient for example when using an `FFT` chain.
  *
  * @param numFrames    number of sample frames for the buffer
  * @param numChannels  number of channels for the buffer
  *
  * @see [[de.sciss.synth.ugen.FFT]]
  */
final case class LocalBuf(numFrames: GE, numChannels: GE = 1)
  extends UGenSource.SingleOut with ScalarRated with IsIndividual {

  protected def makeUGens: UGenInLike = unwrap(this, Vector(numChannels.expand, numFrames.expand))

  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    // by 'visiting' we make sure exactly one instance of `MaxLocalBufs` exists per builder.
    // This is because we use the `MaxLocalBufs` companion object as key. ''Note'' that we
    // do not expand `MaxLocalBufs` at this moment. That is crucial, because we must be
    // able to increase its use count for all `LocalBuf` instances in a synth graph.
    // Instantiating `MaxLocalBufs` adds a lazy element to the builder. This will only be
    // expanded in the _next_ iteration of the builder, thereby freezing the count.
    // To ensure that `MaxLocalBufs` appears before all `LocalBuf` instances within the
    // final UGen graph, `MaxLocalBufs` uses `prependUGen`.
    val maxLocalBufs  = UGenGraph.builder.visit(MaxLocalBufs, MaxLocalBufs())
    val index         = maxLocalBufs.alloc()
    UGen.SingleOut(name, rate, _args :+ Constant(index), isIndividual = true)
  }

  /** Convenience method for `ClearBuf(this)` */
  def clear(): this.type = { ClearBuf(this); this }

  // XXX TODO: def set(values: GE, offset: GE = 0): this.type = { SetBuf(this, values, offset); this }
}

private[synth] final case class MaxLocalBufs() extends UGenSource.SingleOut with ScalarRated with HasSideEffect {
  @transient private[this] var count    = 0
  @transient private[this] var expanded = false

  def alloc(): Int = {
    if (expanded) throw new IllegalStateException(s"$name was already expanded")
    val res = count
    count += 1
    res
  }

  protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = {
    // instead of UGen.SingleOut we do this because we want to _prepend_ not _append_
    val res = new impl.SingleOutImpl(name, rate, _args, isIndividual = false, hasSideEffect = true, specialIndex = 0)
    UGenGraph.builder.prependUGen(res)
    res
  }

  /** Abstract method which must be implemented by creating the actual `UGen`s
    * during expansion. This method is at most called once during graph
    * expansion
    *
    * @return  the expanded object (depending on the type parameter `U`)
    */
  protected def makeUGens: UGenInLike = {
    expanded = true
    unwrap(this, Vector(Constant(count)))
  }
}