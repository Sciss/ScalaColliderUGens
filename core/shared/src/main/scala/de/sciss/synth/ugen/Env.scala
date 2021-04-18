/*
 *  Env.scala
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
package ugen

import de.sciss.synth.Curve.{sine => sin, step => _step, _}
import de.sciss.synth.GEOps.{fromGE => geOps}
import de.sciss.synth.UGenSource.{ProductType, RefMapIn}
import de.sciss.synth.ugen.Env.Segment
import de.sciss.synth.{Curve => SCurve}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

sealed trait EnvFactory[V] extends ProductType[V] {
  import Env.{Segment => Seg}

  protected def create(startLevel: GE, segments: Vec[Seg]): V

  // fixed duration envelopes
  def triangle: V = triangle()

  def triangle(dur: GE = 1f, level: GE = 1f): V = {
    val durH = dur * 0.5f
    create(0, Vector[Seg](durH -> level, durH -> 0))
  }

  def sine: V = sine()

  def sine(dur: GE = 1f, level: GE = 1f): V = {
    val durH = dur * 0.5f
    create(0, Vector[Seg]((durH, level, sin), (durH, 0, sin)))
  }

  def perc: V = perc()

  def perc(attack: GE = 0.01f, release: GE = 1f, level: GE = 1f,
           curve: Env.Curve = parametric(-4f)): V =
    create(0f, Vector[Seg]((attack, level, curve), (release, 0f, curve)))

  def linen: V = linen()

  def linen(attack: GE = 0.01f, sustain: GE = 1f, release: GE = 1f,
            level: GE = 1f, curve: Env.Curve = linear): V =
    create(0f, Vector[Seg]((attack, level, curve), (sustain, level, curve), (release, 0f, curve)))
}

sealed trait EnvLike extends GE {
  def startLevel: GE
  def segments: Seq[Env.Segment]
  def isSustained: Boolean
}

object Env extends EnvFactory[Env] {
  object Curve extends ProductType[Curve] {
    implicit def const(peer: SCurve): Curve = Const(peer)

    implicit def fromDouble(d: Double): Curve = parametric(d.toFloat)

    object Const {
      final val readerKey = "Env$Curve$Const"
    }
    final case class Const(peer: SCurve) extends Curve {
      override def productPrefix: String = Const.readerKey

      override def toString = s"Env.Curve.Const($peer)"

      def id       : GE = Constant(peer.id)
      def curvature: GE = peer match {
        case parametric(c)  => Constant(c)
        case _              => Constant(0f)
      }
    }

    def apply(id: GE, curvature: GE = 0f): Curve = Apply(id, curvature)
    def unapply(s: Curve): Option[(GE, GE)] = Some((s.id, s.curvature))

    object Apply {
      final val productPrefix = "Env$Curve$Apply"
    }
    final case class Apply(id: GE, curvature: GE) extends Curve {
      override def productPrefix: String = Apply.productPrefix

      override def toString = s"Env.Curve($id, $curvature)"
    }

    final val typeId = 265

    override def read(in: RefMapIn, key: String, arity: Int): Curve = {
      key match {
        case Const.`readerKey` =>
          require (arity == 1)
          val _peer = in.readProductT[SCurve]()
          new Const(_peer)

        case Apply.productPrefix =>
          require (arity == 2)
          val _id         = in.readGE()
          val _curvature  = in.readGE()
          new Apply(_id, _curvature)
      }
    }
  }
  sealed trait Curve extends Product {
    def id: GE
    def curvature: GE
  }

  object Segment extends ProductType[Segment] {
    implicit def fromTuple3[D, L, S](tup: (D, L, S))
                                    (implicit durView: D => GE, levelView: L => GE, curveView: S => Curve): Segment =
      Segment(durView(tup._1), levelView(tup._2), curveView(tup._3))

    implicit def fromTuple2[D, L](tup: (D, L))(implicit durView: D => GE, levelView: L => GE): Segment =
      Segment(durView(tup._1), levelView(tup._2), linear)

    final val typeId = 264

    override def read(in: RefMapIn, key: String, arity: Int): Segment = {
      require (arity == 3)
      val _dur          = in.readGE()
      val _targetLevel  = in.readGE()
      val _curve        = in.readProductT[Curve]()
      new Segment(_dur, _targetLevel, _curve)
   }
  }
  final case class Segment(dur: GE, targetLevel: GE, curve: Curve = linear) {
    override def productPrefix = s"Env$$Segment"

    override def toString = s"Env.Segment($dur, $targetLevel, $curve)"
  }

  protected def create(startLevel: GE, segments: Vec[Segment]) = new Env(startLevel, segments)

  // ---- envelopes with sustain ----

  def cutoff: Env = cutoff()

  def cutoff(release: GE = 0.1f, level: GE = 1f, curve: Curve = linear): Env = {
    val releaseLevel: GE = curve match {
      case Curve.Const(`exponential`) => 1e-05f // dbamp( -100 )
      case _ => 0f
    }
    new Env(level, Segment(release, releaseLevel, curve) :: Nil, releaseNode = 0)
  }

  def dadsr: Env = dadsr()

  def dadsr(delay: GE = 0.1f, attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f, release: GE = 1f,
            peakLevel: GE = 1f, curve: Curve = -4.0, bias: GE = 0f): Env =
    new Env(bias, Vector[Segment](
      (delay  , bias                            , curve),
      (attack , peakLevel + bias                , curve),
      (decay  , peakLevel * sustainLevel + bias , curve),
      (release, bias                            , curve)),
      releaseNode = 3)

  def adsr: Env = adsr()

  def adsr(attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f, release: GE = 1f, peakLevel: GE = 1f,
           curve: Curve = -4.0, bias: GE = 0f): Env =
    new Env(bias, Vector[Segment](
      (attack , peakLevel + bias               , curve),
      (decay  , peakLevel * sustainLevel + bias, curve),
      (release, bias                           , curve)),
      releaseNode = 2)

  def asr: Env = asr()

  def asr(attack: GE = 0.01f, level: GE = 1f, release: GE = 1f, curve: Curve = -4.0): Env =
    new Env(0f, Vector[Segment]((attack, level, curve), (release, 0f, curve)), releaseNode = 1)

  def step(levels: Seq[GE], durs: Seq[GE], releaseNode: GE = -99, loopNode: GE = -99 /*, offset: GE = 0 */): Env = {
    require (levels.nonEmpty && levels.size == durs.size)
    new Env(levels.head, (levels, durs).zipped.map { case (lvl, dur) => Segment(dur, lvl, _step) },
      releaseNode = releaseNode, loopNode = loopNode)
  }

  // ---- serialization ----

  final val typeId = 263

  override def read(in: RefMapIn, key: String, arity: Int): Env = {
    require (arity == 4)
    val _startLevel   = in.readGE()
    val _segments     = in.readVec(in.readProductT[Segment]())
    val _releaseNode  = in.readGE()
    val _loopNode     = in.readGE()
    new Env(_startLevel, _segments, _releaseNode, _loopNode)
  }
}

final case class Env(startLevel: GE, segments: Seq[Env.Segment],
                     releaseNode: GE = -99, loopNode: GE = -99)
  extends EnvLike {

  private[synth] def expand: UGenInLike = toGE

  private def toGE: GE = {
    val segmIdx = segments.toIndexedSeq
    val sizeGE: GE = segmIdx.size
    val res: Vec[GE] = startLevel +: sizeGE +: releaseNode +: loopNode +: segmIdx.flatMap(seg =>
      Vector[GE](seg.targetLevel, seg.dur, seg.curve.id, seg.curve.curvature))
    res
  }

  def rate: MaybeRate = toGE.rate

  def isSustained: Boolean = releaseNode != Constant(-99)
}

object IEnv extends EnvFactory[IEnv] {
  protected def create(startLevel: GE, segments: Vec[Env.Segment]) = new IEnv(startLevel, segments)

  // ---- serialization ----

  final val typeId = 269

  override def read(in: RefMapIn, key: String, arity: Int): IEnv = {
    require (arity == 4)
    val _startLevel   = in.readGE()
    val _segments     = in.readVec(in.readProductT[Segment]())
    val _offset       = in.readGE()
    new IEnv(_startLevel, _segments, _offset)
  }
}

final case class IEnv(startLevel: GE, segments: Seq[Env.Segment], offset: GE = 0f)
  extends EnvLike {

  private[synth] def expand: UGenInLike = toGE

  private def toGE: GE = {
    val segmIdx     = segments.toIndexedSeq
    val sizeGE: GE  = segmIdx.size
    val totalDur    = segmIdx.foldLeft[GE](0f)((sum, next) => sum + next.dur)
    val res: Vec[GE] = offset +: startLevel +: sizeGE +: totalDur +: segmIdx.flatMap(seg =>
      Vector[GE](seg.dur, seg.curve.id, seg.curve.curvature, seg.targetLevel))
    res
  }

  def rate: MaybeRate = toGE.rate

  def isSustained = false
}