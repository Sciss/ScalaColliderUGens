/*
 *  SynthGraphBuilderImpl.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package impl

import de.sciss.synth.ugen.ControlProxyLike

import scala.collection.immutable.{IndexedSeq => Vec}

/** The default implementation for `SynthGraph.Builder`. */
final class SynthGraphBuilderImpl extends SynthGraph.Builder {
  private[this] val lazies          = Vec.newBuilder[Lazy]
  private[this] val controlProxies  = Set.newBuilder[ControlProxyLike]

  override def toString = s"SynthGraph.Builder@${hashCode.toHexString}"

  def build: SynthGraph = SynthGraph(lazies.result(), controlProxies.result())

  def addLazy(g: Lazy): Unit = lazies += g

  def addControlProxy(proxy: ControlProxyLike): Unit = controlProxies += proxy
}
