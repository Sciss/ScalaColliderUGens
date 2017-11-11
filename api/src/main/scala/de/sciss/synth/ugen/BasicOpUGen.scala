/*
 *  BasicOpUGen.scala
 *  (ScalaColliderUGens)
 *
 *  Copyright (c) 2008-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import de.sciss.numbers.{LongFunctions => lf, FloatFunctions => rf, FloatFunctions2 => rf2}
import de.sciss.synth.ugen.Constant.{C0, C1, Cm1}
import de.sciss.synth.ugen.{Constant => c}

import scala.annotation.switch
import UGenSource._

/** Unary operations are generally constructed by calling one of the methods of `GEOps`.
  *
  * @see  GEOps
  * @see  BinaryOpUGen
  */
object UnaryOpUGen {
  object Op {
    def apply(id: Int): Op = (id: @switch) match {
      case Neg        .id => Neg
      case Not        .id => Not
      case BitNot     .id => BitNot
      case Abs        .id => Abs
      case Ceil       .id => Ceil
      case Floor      .id => Floor
      case Frac       .id => Frac
      case Signum     .id => Signum
      case Squared    .id => Squared
      case Cubed      .id => Cubed
      case Sqrt       .id => Sqrt
      case Exp        .id => Exp
      case Reciprocal .id => Reciprocal
      case Midicps    .id => Midicps
      case Cpsmidi    .id => Cpsmidi
      case Midiratio  .id => Midiratio
      case Ratiomidi  .id => Ratiomidi
      case Dbamp      .id => Dbamp
      case Ampdb      .id => Ampdb
      case Octcps     .id => Octcps
      case Cpsoct     .id => Cpsoct
      case Log        .id => Log
      case Log2       .id => Log2
      case Log10      .id => Log10
      case Sin        .id => Sin
      case Cos        .id => Cos
      case Tan        .id => Tan
      case Asin       .id => Asin
      case Acos       .id => Acos
      case Atan       .id => Atan
      case Sinh       .id => Sinh
      case Cosh       .id => Cosh
      case Tanh       .id => Tanh
      case Distort    .id => Distort
      case Softclip   .id => Softclip
      case Ramp       .id => Ramp
      case Scurve     .id => Scurve
    }
  }

  sealed trait Op extends Product {
    op =>

    def id: Int

    final def make(a: GE): GE = a match {
      case c(f) => c(make1(f))
      case _    => new UnaryOpUGen(/* a.rate, */ op, a)
    }

    final def make1(a: UGenIn): UGenIn = a match {
      case c(f) => c(make1(f))
      case _    => UGenImpl(op, a)
    }

    def make1(a: Float): Float

    override def productPrefix  = s"UnaryOpUGen$$Op"
    override def productArity   = 1
    override def productElement(n: Int): Any = if (n == 0) id else throw new IndexOutOfBoundsException(n.toString)

    def name: String = plainName.capitalize
    def prefix: Boolean = false
    
    private def plainName: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      cn.substring(i, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  case object Neg extends Op {
    final val id = 0
    override val name = "-"
    override def prefix = true
    def make1(a: Float): Float = -a
  }

  case object Not extends Op {
    final val id = 1
    override val name = "!"
    override def prefix = true
    def make1(a: Float): Float = rf2.not(a)
  }

  // case object IsNil       extends Op(  2 )
  // case object NotNil      extends Op(  3 )
  case object BitNot extends Op {
    final val id = 4
    def make1(a: Float): Float = ~a.toInt
  }
  case object Abs extends Op {
    final val id = 5
    def make1(a: Float): Float = rf.abs(a)
  }

  // case object ToFloat     extends Op(  6 )
  // case object ToInt       extends Op(  7 )
  case object Ceil extends Op {
    final val id = 8
    def make1(a: Float): Float = rf.ceil(a)
  }

  case object Floor extends Op {
    final val id = 9
    def make1(a: Float): Float = rf.floor(a)
  }

  case object Frac extends Op {
    final val id = 10
    def make1(a: Float): Float = rf.frac(a)
  }

  case object Signum extends Op {
    final val id = 11
    def make1(a: Float): Float = math.signum(a)
  }

  case object Squared extends Op {
    final val id = 12
    def make1(a: Float): Float = rf.squared(a)
  }

  case object Cubed extends Op {
    final val id = 13
    def make1(a: Float): Float = rf2.cubed(a)
  }

  case object Sqrt extends Op {
    final val id = 14
    def make1(a: Float): Float = rf.sqrt(a)
  }

  case object Exp extends Op {
    final val id = 15
    def make1(a: Float): Float = rf.exp(a)
  }

  case object Reciprocal extends Op {
    final val id = 16
    def make1(a: Float): Float = rf2.reciprocal(a)
  }

  case object Midicps extends Op {
    final val id = 17
    def make1(a: Float): Float = rf.midicps(a)
  }

  case object Cpsmidi extends Op {
    final val id = 18
    def make1(a: Float): Float = rf.cpsmidi(a)
  }

  case object Midiratio extends Op {
    final val id = 19
    def make1(a: Float): Float = rf.midiratio(a)
  }

  case object Ratiomidi extends Op {
    final val id = 20
    def make1(a: Float): Float = rf.ratiomidi(a)
  }

  case object Dbamp extends Op {
    final val id = 21
    def make1(a: Float): Float = rf.dbamp(a)
  }

  case object Ampdb extends Op {
    final val id = 22
    def make1(a: Float): Float = rf.ampdb(a)
  }

  case object Octcps extends Op {
    final val id = 23
    def make1(a: Float): Float = rf.octcps(a)
  }

  case object Cpsoct extends Op {
    final val id = 24
    def make1(a: Float): Float = rf.cpsoct(a)
  }

  case object Log extends Op {
    final val id = 25
    def make1(a: Float): Float = rf.log(a)
  }

  case object Log2 extends Op {
    final val id = 26
    def make1(a: Float): Float = rf.log2(a)
  }

  case object Log10 extends Op {
    final val id = 27
    def make1(a: Float): Float = rf.log10(a)
  }

  case object Sin extends Op {
    final val id = 28
    def make1(a: Float): Float = rf.sin(a)
  }

  case object Cos extends Op {
    final val id = 29
    def make1(a: Float): Float = rf.cos(a)
  }

  case object Tan extends Op {
    final val id = 30
    def make1(a: Float): Float = rf.tan(a)
  }

  case object Asin extends Op {
    final val id = 31
    def make1(a: Float): Float = rf.asin(a)
  }

  case object Acos extends Op {
    final val id = 32
    def make1(a: Float): Float = rf.acos(a)
  }

  case object Atan extends Op {
    final val id = 33
    def make1(a: Float): Float = rf.atan(a)
  }

  case object Sinh extends Op {
    final val id = 34
    def make1(a: Float): Float = rf.sinh(a)
  }

  case object Cosh extends Op {
    final val id = 35
    def make1(a: Float): Float = rf.cosh(a)
  }

  case object Tanh extends Op {
    final val id = 36
    def make1(a: Float): Float = rf.tanh(a)
  }

  // class Rand              extends Op( 37 )
  // class Rand2             extends Op( 38 )
  // class Linrand           extends Op( 39 )
  // class Bilinrand         extends Op( 40 )
  // class Sum3rand          extends Op( 41 )
  case object Distort extends Op {
    final val id = 42
    def make1(a: Float): Float = rf2.distort(a)
  }

  case object Softclip extends Op {
    final val id = 43
    def make1(a: Float): Float = rf2.softclip(a)
  }

  // class Coin              extends Op( 44 )
  // case object DigitValue  extends Op( 45 )
  // case object Silence     extends Op( 46 )
  // case object Thru        extends Op( 47 )
  // case object RectWindow  extends Op( 48 )
  // case object HanWindow   extends Op( 49 )
  // case object WelWindow   extends Op( 50 )
  // case object TriWindow   extends Op( 51 )
  case object Ramp extends Op {
    final val id = 52
    def make1(a: Float): Float = rf2.ramp(a)
  }

  case object Scurve extends Op {
    final val id = 53
    def make1(a: Float): Float = rf2.scurve(a)
  }

  // Note: only deterministic selectors are implemented!!
  private def UGenImpl(selector: Op, a: UGenIn): UGen.SingleOut =
      UGen.SingleOut("UnaryOpUGen", a.rate, Vector(a),
        specialIndex = selector.id)
}

final case class UnaryOpUGen(selector: UnaryOpUGen.Op, a: GE)
  extends UGenSource.SingleOut {

  def rate: MaybeRate = a.rate

  protected def makeUGens: UGenInLike = unwrap(this, Vector(a))

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val a = args.head
    selector.make1(a)
  }

  override def toString: String = if (selector.prefix)
    s"(${selector.name}$a)"
  else
    s"$a.${selector.name}"
}

/** Binary operations are generally constructed by calling one of the methods of `GEOps`.
  *
  * @see  GEOps
  * @see  UnaryOpUGen
  */
object BinaryOpUGen {
  // note: this is not optimizing, as would be `op.make(a, b)`, because it guarantees that the return
  // type is BinaryOpUGen. this is used in deserialization, you should prefer `op.make` instead.
  def apply(op: Op, a: GE, b: GE): BinaryOpUGen = op match {
    case Firstarg => Impure(op, a, b)
    case _        => Pure  (op, a, b)
  }

  def unapply(b: BinaryOpUGen): Option[(Op, GE, GE)] = Some((b.selector, b.a, b.b))

  object Op {
    def apply(id: Int): Op = (id: @switch) match {
      case Plus     .id => Plus
      case Minus    .id => Minus
      case Times    .id => Times
      case Div      .id => Div
      case Mod      .id => Mod
      case Eq       .id => Eq
      case Neq      .id => Neq
      case Lt       .id => Lt
      case Gt       .id => Gt
      case Leq      .id => Leq
      case Geq      .id => Geq
      case Min      .id => Min
      case Max      .id => Max
      case BitAnd   .id => BitAnd
      case BitOr    .id => BitOr
      case BitXor   .id => BitXor
      case Lcm      .id => Lcm
      case Gcd      .id => Gcd
      case RoundTo  .id => RoundTo
      case RoundUpTo.id => RoundUpTo
      case Trunc    .id => Trunc
      case Atan2    .id => Atan2
      case Hypot    .id => Hypot
      case Hypotx   .id => Hypotx
      case Pow      .id => Pow
      case LeftShift.id => LeftShift
      case RightShift.id => RightShift
      case Ring1    .id => Ring1
      case Ring2    .id => Ring2
      case Ring3    .id => Ring3
      case Ring4    .id => Ring4
      case Difsqr   .id => Difsqr
      case Sumsqr   .id => Sumsqr
      case Sqrsum   .id => Sqrsum
      case Sqrdif   .id => Sqrdif
      case Absdif   .id => Absdif
      case Thresh   .id => Thresh
      case Amclip   .id => Amclip
      case Scaleneg .id => Scaleneg
      case Clip2    .id => Clip2
      case Excess   .id => Excess
      case Fold2    .id => Fold2
      case Wrap2    .id => Wrap2
      case Firstarg .id => Firstarg
    }
  }

  import UnaryOpUGen.{Neg, Reciprocal}

  sealed trait Op extends Product {
    op =>

    def id: Int
    
    def infix: Boolean = false

    //      def make( rate: R, a: GE[ UGenIn[ R ]]) = UnaryOp[ R ]( rate, this, a )
    def make(a: GE, b: GE): GE = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _ => Pure(op, a, b)
    }

    protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => UGenImpl(op, a, b, hasSideEffect = false)
    }

    def make1(a: Float, b: Float): Float

    override def productPrefix  = s"BinaryOpUGen$$Op"
    override def productArity   = 1
    override def productElement(n: Int): Any = if (n == 0) id else throw new IndexOutOfBoundsException(n.toString)

    def name: String = plainName.capitalize

    private def plainName: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      cn.substring(i, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  case object Plus extends Op {
    final val id = 0
    override val name = "+"
    override def infix = true

    def make1(a: Float, b: Float): Float = rf.+(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (C0, _)  => b
      case (_, C0)  => a
      case _        => super.make1(a, b)
    }
  }

  case object Minus extends Op {
    final val id = 1
    override val name = "-"
    override def infix = true

    def make1(a: Float, b: Float): Float = rf.-(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (C0, _) => Neg.make1(b)
      case (_, C0) => a
      case _ => super.make1(a, b)
    }
  }

  case object Times extends Op {
    final val id = 2
    override val name = "*"
    override def infix = true

    def make1(a: Float, b: Float): Float = rf.*(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (C0, _)  => a
      case (_, C0)  => b
      case (C1, _)  => b
      case (_, C1)  => a
      case (Cm1, _) => Neg.make1(b) // -b
      case (_, Cm1) => Neg.make1(a) // -a
      case _        => super.make1(a, b)
    }
  }

  // case object IDiv           extends Op(  3 )
  case object Div extends Op {
    final val id = 4
    override val name = "/"
    override def infix = true

    def make1(a: Float, b: Float): Float = rf./(a, b)

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (_, C1)  => a
      case (_, Cm1) => Neg.make1(a) // -a
      case _          =>
        if (b.rate == scalar) {
          Times.make1(a, Reciprocal.make1(b))
        } else {
          super.make1(a, b)
        }
    }
  }

  case object Mod extends Op {
    final val id = 5
    override val name = "%"
    override def infix = true
    def make1(a: Float, b: Float): Float = rf.%(a, b)
  }

  case object Eq extends Op {
    final val id = 6
    override val name = "sig_=="
    override def infix = true
    def make1(a: Float, b: Float): Float = if( a == b ) 1 else 0
  }

  case object Neq extends Op {
    final val id = 7
    override val name = "sig_!="
    override def infix = true
    def make1(a: Float, b: Float): Float = if( a != b ) 1 else 0
  }

  case object Lt extends Op {
    final val id = 8
    override val name = "<"
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a < b) 1f else 0f // NOT rf.< !
  }

  case object Gt extends Op {
    final val id = 9
    override val name = ">"
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a > b) 1f else 0f // NOT rf.> !
  }

  case object Leq extends Op {
    final val id = 10
    override val name = "<="
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a <= b) 1f else 0f // NOT rf.<= !
  }

  case object Geq extends Op {
    final val id = 11
    override val name = ">="
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a >= b) 1f else 0f // NOT rf.>= !
  }

  case object Min extends Op {
    final val id = 12
    def make1(a: Float, b: Float): Float = rf.min(a, b)
  }

  case object Max extends Op {
    final val id = 13
    def make1(a: Float, b: Float): Float = rf.max(a, b)
  }

  case object BitAnd extends Op {
    final val id = 14
    override def infix = true
    override val name = "&"
    def make1(a: Float, b: Float): Float = rf.&(a, b)
  }

  case object BitOr extends Op {
    final val id = 15
    override def infix = true
    override val name = "|"
    def make1(a: Float, b: Float): Float = rf.|(a, b)
  }

  case object BitXor extends Op {
    final val id = 16
    override def infix = true
    override val name = "^"
    def make1(a: Float, b: Float): Float = rf.^(a, b)
  }

  case object Lcm extends Op {
    final val id = 17
    def make1(a: Float, b: Float): Float = lf.lcm(a.toLong, b.toLong).toFloat
  }

  case object Gcd extends Op {
    final val id = 18
    def make1(a: Float, b: Float): Float = lf.gcd(a.toLong, b.toLong).toFloat
  }

  case object RoundTo extends Op {
    final val id = 19
    def make1(a: Float, b: Float): Float = rf.roundTo(a, b)
  }

  case object RoundUpTo extends Op {
    final val id = 20
    def make1(a: Float, b: Float): Float = rf.roundUpTo(a, b)
  }

  case object Trunc extends Op {
    final val id = 21
    def make1(a: Float, b: Float): Float = rf.trunc(a, b)
  }

  case object Atan2 extends Op {
    final val id = 22
    def make1(a: Float, b: Float): Float = rf.atan2(a, b)
  }

  case object Hypot extends Op {
    final val id = 23
    def make1(a: Float, b: Float): Float = rf.hypot(a, b)
  }

  case object Hypotx extends Op {
    final val id = 24
    def make1(a: Float, b: Float): Float = rf.hypotx(a, b)
  }

  /** '''Warning:''' Unlike a normal power operation, the signum of the
    * left operand is always preserved. I.e. `DC.kr(-0.5).pow(2)` will
    * not output `0.25` but `-0.25`. This is to avoid problems with
    * floating point noise and negative input numbers, so
    * `DC.kr(-0.5).pow(2.001)` does not result in a `NaN`, for example.
    */
  case object Pow extends Op {
    final val id = 25
    def make1(a: Float, b: Float): Float = rf.pow(a, b)
  }

  case object LeftShift extends Op {
    final val id = 26
    override def infix = true
    override val name = "<<"
    def make1(a: Float, b: Float): Float = (a.toLong << b.toInt).toFloat
  }

  case object RightShift extends Op {
    final val id = 27
    override def infix = true
    override val name = ">>"
    def make1(a: Float, b: Float): Float = (a.toLong >> b.toInt).toFloat
  }
  // case object UnsgnRghtShft  extends Op( 28 )
  // case object Fill           extends Op( 29 )
  case object Ring1 extends Op {
    final val id = 30
    def make1(a: Float, b: Float): Float = rf2.ring1(a, b)
  }

  case object Ring2 extends Op {
    final val id = 31
    def make1(a: Float, b: Float): Float = rf2.ring2(a, b)
  }

  case object Ring3 extends Op {
    final val id = 32
    def make1(a: Float, b: Float): Float = rf2.ring3(a, b)
  }

  case object Ring4 extends Op {
    final val id = 33
    def make1(a: Float, b: Float): Float = rf2.ring4(a, b)
  }

  case object Difsqr extends Op {
    final val id = 34
    def make1(a: Float, b: Float): Float = rf.difsqr(a, b)
  }

  case object Sumsqr extends Op {
    final val id = 35
    def make1(a: Float, b: Float): Float = rf.sumsqr(a, b)
  }

  case object Sqrsum extends Op {
    final val id = 36
    def make1(a: Float, b: Float): Float = rf.sqrsum(a, b)
  }

  case object Sqrdif extends Op {
    final val id = 37
    def make1(a: Float, b: Float): Float = rf.sqrdif(a, b)
  }

  case object Absdif extends Op {
    final val id = 38
    def make1(a: Float, b: Float): Float = rf.absdif(a, b)
  }

  case object Thresh extends Op {
    final val id = 39
    def make1(a: Float, b: Float): Float = rf2.thresh(a, b)
  }

  case object Amclip extends Op {
    final val id = 40
    def make1(a: Float, b: Float): Float = rf2.amclip(a, b)
  }

  case object Scaleneg extends Op {
    final val id = 41
    def make1(a: Float, b: Float): Float = rf2.scaleneg(a, b)
  }

  case object Clip2 extends Op {
    final val id = 42
    def make1(a: Float, b: Float): Float = rf.clip2(a, b)
  }

  case object Excess extends Op {
    final val id = 43
    def make1(a: Float, b: Float): Float = rf.excess(a, b)
  }

  case object Fold2 extends Op {
    final val id = 44
    def make1(a: Float, b: Float): Float = rf.fold2(a, b)
  }

  case object Wrap2 extends Op {
    final val id = 45
    def make1(a: Float, b: Float): Float = rf.wrap2(a, b)
  }

  case object Firstarg extends Op {
    final val id = 46

    override def make(a: GE, b: GE): GE = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _ => Impure(this, a, b)
    }

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => UGenImpl(this, a, b, hasSideEffect = true)
    }

    def make1(a: Float, b: Float): Float = a
  }

  // case object Rrand          extends Op( 47 )
  // case object ExpRRand       extends Op( 48 )

  private final case class Pure(/* rate: MaybeRate, */ selector: Op, a: GE, b: GE)
    extends BinaryOpUGen

  private final case class Impure(/* rate: MaybeRate, */ selector: Op, a: GE, b: GE)
    extends BinaryOpUGen with HasSideEffect {
  }

  // Note: only deterministic selectors are implemented!!
  private[this] def UGenImpl(selector: Op, a: UGenIn, b: UGenIn, hasSideEffect: Boolean): UGen.SingleOut =
      UGen.SingleOut("BinaryOpUGen", a.rate max b.rate, Vector(a, b),
        hasSideEffect = hasSideEffect, specialIndex = selector.id)
}

// XXX TODO - this could become private once the op's make method return type is changed to GE
sealed trait BinaryOpUGen extends UGenSource.SingleOut {
  def selector: BinaryOpUGen.Op
  def a: GE
  def b: GE

  override final def productPrefix = "BinaryOpUGen"

  //  final def productElement(n: Int): Any = (n: @switch) match {
  //    case 0 => selector
  //    case 1 => a
  //    case 2 => b
  //  }

  final def rate: MaybeRate = MaybeRate.max_?( a.rate, b.rate )

  protected final def makeUGens: UGenInLike = unwrap(this, Vector(a.expand, b.expand))

  protected final def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val a0 = args(0)
    val a1 = args(1)
    selector.make1(a0, a1)
  }

  override def toString: String = if (selector.infix)
    s"($a ${selector.name} $b)"
  else
    s"$a.${selector.name}($b)"
}