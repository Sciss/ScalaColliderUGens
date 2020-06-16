/*
 *  Rate.scala
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

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}

object MaybeRate {
  /** Calculates the maximum rate among a list of optional rates. If the list is empty or contains
    * at least one `UndefinedRate`, the return value is also `UndefinedRate`.
    */
  def max_?(rates: MaybeRate*): MaybeRate =
    if (rates.isEmpty) UndefinedRate else {
      var res: Rate = scalar
      val it = rates.iterator
      while (it.hasNext) it.next() match {
        case UndefinedRate => return UndefinedRate
        case r: Rate => if (r.id > res.id) res = r
      }
      res
    }

  /** Calculates the minimum rate among a list of optional rates. If the list is empty or contains
    * at least one `UndefinedRate`, the return value is also `UndefinedRate`.
    */
  def min_?(rates: MaybeRate*): MaybeRate =
    if (rates.isEmpty) UndefinedRate else {
      var res: Rate = demand
      val it = rates.iterator
      while (it.hasNext) it.next() match {
        case UndefinedRate => return UndefinedRate
        case r: Rate => if (r.id < res.id) res = r
      }
      res
    }

  /** Reduces a list of optional rates to one value. If the list is empty or contains different rates,
    * the return value is `UndefinedRate`. Otherwise, if all elements are equal, returns that one rate.
    */
  def reduce(rates: MaybeRate*): MaybeRate = {
    rates.headOption match {
      case Some(r) if rates.forall(_ == r) => r
      case _                               => UndefinedRate
    }
  }

  /** Constructs an optional rate from a given identifier. `-1` denotes the `UndefinedRate`, values `>= 0`
    * denote the identifier of a defined rate.
    */
  def apply(id: Int): MaybeRate = if (id == -1) UndefinedRate else Rate(id)
}

/** This trait denotes an optional server calculation rate. Either the rate is explicit, [[de.sciss.synth.Rate Rate]],
  * or it is undefined, [[de.sciss.synth.UndefinedRate UndefinedRate]]. In the latter case, a rate is implicitly
  * derived from the input arguments of a UGen.
  */
sealed abstract class MaybeRate extends Product {
  /** The identifier of a `MaybeRate` is either `-1` (undefined) or the identifier of a defined rate (`>= 0`). */
  def id: Int
  final def name: String = productPrefix
  /** Lifts this optional rate into an `Option[Rate]`. An `UndefinedRate` becomes `None`, a define `Rate`
    * becomes `Some`. */
  def toOption: Option[Rate]

  /** A `getOrElse` operator that returns a defined rate, and if this object is `UndefinedRate` will evaluate the
    * method argument. */
  def getOrElse(r: => Rate): Rate
}

/** An undefined rate signifies that a rate is either unknown or will be implicitly resolved. */
case object UndefinedRate extends MaybeRate {
  final val id = -1
  val toOption: Option[Rate] = None
  def getOrElse(r: => Rate): Rate = r
}

object Rate {
  /** Rates are implicitly ordered by their identifiers. */
  implicit val ordering: Ordering[Rate] = Ordering.ordered[Rate]

  /** Constructs a rate from a given identifier. */
  def apply(id: Int): Rate = (id: @switch) match {
    case scalar   .id => scalar
    case control  .id => control
    case audio    .id => audio
    case demand   .id => demand
  }

  sealed abstract class Bus extends Rate
}
/** The server calculation rate of a UGen or a UGen output.
  *
  * The following rates are known:
  *
  *  - [[de.sciss.synth.scalar scalar]] (only calculated once when a `Synth` is initialized.
  *  - [[de.sciss.synth.control control]] (one value per block)
  *  - [[de.sciss.synth.audio audio]] (full audio sample rate)
  *  - [[de.sciss.synth.demand demand]] (calculation specially triggered on demand)
  */
sealed abstract class Rate extends MaybeRate with Ordered[Rate] {
  def methodName: String

  /** Returns `Some(this`). */
  final val toOption    : Option[Rate] = Some (this)
  final val toIndexedSeq: Vec   [Rate] = Vec  (this)

  /** Returns `this` object without resolving the argument. */
  final def getOrElse(r: => Rate): Rate = this

  /** Returns the minimum of this and another rate, based on their identifiers
    * (e.g., `scalar < control`).
    */
  final def min(that: Rate): Rate = if (id < that.id) this else that
  /** Returns the maximum of this and another rate, based on their identifiers
    * (e.g., `control > scalar`).
    */
  final def max(that: Rate): Rate = if (id > that.id) this else that

  /** Compares this and another rate, based on their identifiers
    * (e.g., `scalar compare control == -1` and `audio compare audio == 0`).
    */
  final def compare(that: Rate): Int = id - that.id
}

/** Scalar rated calculation (id `0`) means that a value is only calculated once when a `Synth` is initialized. */
case object scalar extends Rate {
  final val id = 0
  final val methodName = "ir"
}

/** Control rated calculation (id `1`) means that one value is calculated per block. With a default block size
  * of `64`, for every 64 audio samples one control value is calculated. Thus, if the sampling rate is 44.1 kHz,
  * the control rate would be 44100/64 = approx. 689 per second. */
case object control extends Rate.Bus {
  final val id = 1
  final val methodName = "kr"
}

/** Audio rated calculation (id `1`) means that values are calculated at the audio sampling rate. For example,
  * if the server and sound hardware run at 44.1 kHz, then 44100 samples are calculated per second. On the server,
  * audio rate calculation is performed in chunks, depending on the block size setting. */
case object audio extends Rate.Bus {
  final val id = 2
  final val methodName = "ar"
}

/** Demand rated calculation (id `1`) means that the UGen is queried by trigger through a special UGen such as
  * `Demand`.
  */
case object demand extends Rate {
  final val id = 3
  final val methodName = "dr"
}

/** Utility trait that defines a `rate` method returning `scalar`. */
trait ScalarRated  { final def rate: Rate = scalar  }
/** Utility trait that defines a `rate` method returning `control`. */
trait ControlRated { final def rate: Rate = control }
/** Utility trait that defines a `rate` method returning `audio`. */
trait AudioRated   { final def rate: Rate = audio   }
/** Utility trait that defines a `rate` method returning `demand`. */
trait DemandRated  { final def rate: Rate = demand  }
