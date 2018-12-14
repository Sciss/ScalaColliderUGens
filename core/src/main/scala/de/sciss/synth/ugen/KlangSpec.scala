/*
 *  KlangSpec.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import scala.collection.immutable.{IndexedSeq => Vec}

object KlangSpec {
  def fill(n: Int)(thunk: => (GE, GE, GE)): Seq =
    Seq(Vec.fill[(GE, GE, GE)](n)(thunk).map(tup => KlangSpec(tup._1, tup._2, tup._3)))

  def tabulate(n: Int)(func: Int => (GE, GE, GE)): Seq =
    Seq(Vec.tabulate[(GE, GE, GE)](n)(func).map(tup => KlangSpec(tup._1, tup._2, tup._3)))

  final case class Seq(elems: Vec[KlangSpec]) extends GE {
    override def productPrefix: String      = s"KlangSpec$$Seq"
    def numOutputs            : Int         = elems.size * 3
    def rate                  : MaybeRate   = MaybeRate.reduce(elems.map(_.rate): _*)
    override def toString     : String      = elems.mkString(s"$productPrefix(", ",", ")")

    def expand                : UGenInLike  = UGenInGroup(elems.flatMap(_.expand.outputs))
  }
}

final case class KlangSpec(freq: GE, amp: GE = 1, decay: GE = 0) extends GE {
  def numOutputs: Int         = 3
  def rate      : MaybeRate   = MaybeRate.reduce(freq.rate, amp.rate, decay.rate)

  def expand    : UGenInLike  = UGenInGroup(Vec(freq.expand, amp.expand, decay.expand))
}
