/*
 *  WireBufAllocator.scala
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
package impl

private[synth] final class WireBufAllocator {
  private[this] var refsMaxSize   = 32
  private[this] var stackMaxSize  = 32
  private[this] var refs          = new Array[Int](refsMaxSize)
  private[this] var stack         = new Array[Int](stackMaxSize)
  private[this] var stackPtr      = 0
  private[this] var nextIndex     = 0

  def size(): Int = nextIndex

  def alloc(count: Int): Int = {
    val outIndex: Int = if (stackPtr > 0) {
      stackPtr -= 1
      stack(stackPtr)
    } else {
      val res = nextIndex
      nextIndex += 1
      res
    }
    if (outIndex >= refsMaxSize) {
      refsMaxSize *= 2
      val newRefs = new Array[Int](refsMaxSize)
      System.arraycopy(refs, 0, newRefs, 0, refs.length)
      refs = newRefs
    }
    refs(outIndex) = count
    outIndex
  }

  def release(inIndex: Int): Unit = {
    val count = refs(inIndex) - 1
    if (count < 0) throw new IllegalStateException("buffer coloring error")

    refs(inIndex) = count
    if (count == 0) {
      if (stackPtr >= stackMaxSize) {
        stackMaxSize *= 2
        val newStack = new Array[Int](stackMaxSize)
        System.arraycopy(stack, 0, newStack, 0, stack.length)
        stack = newStack
      }
      stack(stackPtr) = inIndex
      stackPtr += 1
    }
  }
}
