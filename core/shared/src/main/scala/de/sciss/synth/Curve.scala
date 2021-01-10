/*
 *  Curve.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2021 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

import de.sciss.serial.{ConstFormat, DataInput, DataOutput}
import de.sciss.synth.UGenSource.{ProductReader, RefMapIn}

import scala.annotation.switch
import scala.language.implicitConversions
import scala.math.{Pi, abs, cos, pow, sin, sqrt}

object Curve extends ProductReader[Curve] {
  case object step extends Curve {
    final val id = 0

    override final val readerKey  = "Curve$step"  // compatible with SoundProcesses serialization
    override def toString         = "step"

    def levelAt(pos: Float, y1: Float, y2: Float): Float = if (pos < 1f) y1 else y2
  }

  case object linear extends Curve {
    final val id = 1

    override final val readerKey  = "Curve$linear"
    override def toString         = "linear"

    def levelAt(pos: Float, y1: Float, y2: Float): Float = pos * (y2 - y1) + y1
  }
  val lin: linear.type = linear

  case object exponential extends Curve {
    final val id = 2

    override final val readerKey  = "Curve$exponential"
    override def toString         = "exponential"

    def levelAt(pos: Float, y1: Float, y2: Float): Float =
      if (y1 == 0) {
        if (pos >= 0.5) y2 else y1
      } else {
        val y1d = y1.toDouble
        val y2d = y2.toDouble
        (y1d * pow(y2d / y1d, pos)).toFloat
      }
  }
  val exp: exponential.type = exponential

  case object sine extends Curve {
    final val id = 3

    override final val readerKey  = "Curve$sine"
    override def toString         = "sine"

    def levelAt(pos: Float, y1: Float, y2: Float): Float =
      (y1 + (y2 - y1) * (-cos(Pi * pos) * 0.5 + 0.5)).toFloat
  }

  case object welch extends Curve {
    final val id = 4

    override final val readerKey  = "Curve$welch"
    override def toString         = "welch"

    def levelAt(pos: Float, y1: Float, y2: Float): Float = if (y1 < y2) {
      (y1 + (y2 - y1) * sin(Pi * 0.5 * pos)).toFloat
    } else {
      (y2 - (y2 - y1) * sin(Pi * 0.5 * (1 - pos))).toFloat
    }
  }

  implicit def fromDouble(d: Double): Curve = parametric(d.toFloat)

  object parametric {
    final val id = 5

    final val readerKey = s"Curve$$parametric"
  }
  final case class parametric(/*override val */ curvature: Float) extends Curve {
    def id: Int = parametric.id

    override def readerKey: String  = parametric.readerKey // s"Curve$$parametric"
    override def toString           = s"parametric($curvature)"

    def levelAt(pos: Float, y1: Float, y2: Float): Float = if (abs(curvature) < 0.0001f) {
      pos * (y2 - y1) + y1
    } else {
      val denominator = 1.0 - math.exp(curvature)
      val numerator   = 1.0 - math.exp(pos * curvature)
      (y1 + (y2 - y1) * (numerator / denominator)).toFloat
    }
  }

  case object squared extends Curve {
    final val id = 6

    override final val readerKey  = "Curve$squared"
    override def toString         = "squared"

    def levelAt(pos: Float, y1: Float, y2: Float): Float = {
      val y1Pow2  = sqrt(y1)
      val y2Pow2  = sqrt(y2)
      val yPow2   = pos * (y2Pow2 - y1Pow2) + y1Pow2
      (yPow2 * yPow2).toFloat
    }
  }

  case object cubed extends Curve {
    final val id = 7

    override final val readerKey  = "Curve$cubed"
    override def toString         = "cubed"

    def levelAt(pos: Float, y1: Float, y2: Float): Float = {
      val y1Pow3  = pow(y1, 0.3333333)
      val y2Pow3  = pow(y2, 0.3333333)
      val yPow3   = pos * (y2Pow3 - y1Pow3) + y1Pow3
      (yPow3 * yPow3 * yPow3).toFloat
    }
  }

  implicit object format extends ConstFormat[Curve] {
    def write(shape: Curve, out: DataOutput): Unit = {
      out.writeInt(shape.id)
      shape match {
        case parametric(c)  => out.writeFloat(c)
        case _              =>
      }
    }

    def read(in: DataInput): Curve =
      (in.readInt(): @switch) match {
        case step       .id => step
        case linear     .id => linear
        case exponential.id => exponential
        case sine       .id => sine
        case welch      .id => welch
        case parametric .id => parametric(in.readFloat())
        case squared    .id => squared
        case cubed      .id => cubed
        case other          => sys.error(s"Unexpected envelope shape id $other")
      }
  }

  override def read(in: RefMapIn, prefix: String, arity: Int): Curve =
    prefix match {
      case step       .readerKey => step
      case linear     .readerKey => linear
      case exponential.readerKey => exponential
      case sine       .readerKey => sine
      case welch      .readerKey => welch

      case parametric .readerKey =>
        require (arity == 1)
        val _curvature = in.readFloat()
        new parametric(_curvature)

      case squared    .readerKey => squared
      case cubed      .readerKey => cubed
      case _ =>
        sys.error(s"Unexpected product prefix '$prefix'")
    }
}
sealed trait Curve extends Product {
  def id: Int

  override final def productPrefix: String = readerKey

  def readerKey: String

  def levelAt(pos: Float, y1: Float, y2: Float): Float
}