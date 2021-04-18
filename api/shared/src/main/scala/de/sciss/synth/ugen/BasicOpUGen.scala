/*
 *  BasicOpUGen.scala
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

import de.sciss.numbers.{FloatFunctions => rf, FloatFunctions2 => rf2, LongFunctions => lf}
import de.sciss.synth.UGenSource._
import de.sciss.synth.ugen.Constant.{C0, C1, Cm1}
import de.sciss.synth.ugen.{Constant => c}

import scala.annotation.switch

/** Unary operations are generally constructed by calling one of the methods of `GEOps`.
  *
  * @see  GEOps
  * @see  BinaryOpUGen
  */
object UnaryOpUGen extends ProductType[UnaryOpUGen] {
  // note: this is not optimizing, as would be `op.make(a)`, because it guarantees that the return
  // type is UnaryOpUGen. this is used in deserialization, you should prefer `op.make` instead.
  def apply(op: Op, a: GE): UnaryOpUGen = op.makeNoOptimization(a)

  def unapply(b: UnaryOpUGen): Option[(Op, GE)] = Some((b.selector, b.a))

  final val typeId = 442

  object Op extends ProductType[Op] {
    final val typeId = 443

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
      case Rand       .id => Rand
      case Rand2      .id => Rand2
      case Linrand    .id => Linrand
      case Bilinrand  .id => Bilinrand
      case Sum3rand   .id => Sum3rand
      case Distort    .id => Distort
      case Softclip   .id => Softclip
      case Coin       .id => Coin
      // case DigitValue ?
      // case Silence ?
      // case Thru ?
      case RectWindow .id => RectWindow
      case HannWindow .id => HannWindow
      case WelchWindow.id => WelchWindow
      case TriWindow  .id => TriWindow
      case Ramp       .id => Ramp
      case Scurve     .id => Scurve
    }

    override def read(in: RefMapIn, key: String, arity: Int): Op = {
      require (arity == 1)
      val _id = in.readInt()
      Op(_id)
    }
  }

  private def uncapitalize(in: String): String = if (in.isEmpty) in else {
    val c0 = in.charAt(0)
    if (Character.isUpperCase(c0)) s"${Character.toLowerCase(c0)}${in.substring(1)}" else in
  }

  sealed abstract class Op extends Product {
    op =>

    def id: Int

    def make(a: GE): GE

    def make1(a: UGenIn): UGenIn

    def makeNoOptimization(a: GE): UnaryOpUGen

    override final def productPrefix  = s"UnaryOpUGen$$Op"
    override final def productArity   = 1
    override final def productElement(n: Int): Any = if (n == 0) id else throw new IndexOutOfBoundsException(n.toString)

    /** The name one would use to display the operator in
      * a user friendly way.
      * For example, for `Neg` this would be `-`.
      */
    def name: String = uncapitalize(plainName)

    /** Whether the operator should be written in prefix position.
      * For example, `Ampdb` is '''not''' in prefix position, you would write
      * `signal.ampDb`, whereas `Neg` is prefix position, as you would write
      * `-signal`.
      */
    def prefix: Boolean = false

    /** The name one would call on `GEOps` to obtain this operator.
      * For example, for `Neg` this would be `unary_-`.
      */
    def methodName: String =
      if (prefix) s"unary_$name" else name
    
    private final def plainName: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      cn.substring(i, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  sealed abstract class PureOp extends Op {
    op =>

    final def make(a: GE): GE = a match {
      case c(f) => c(make1(f))
      case _    => Pure(op, a)
    }

    final def make1(a: UGenIn): UGenIn = a match {
      case c(f) => c(make1(f))
      case _    => UGenImpl(op, a, isIndividual = false, hasSideEffect = false)
    }

    final def makeNoOptimization(a: GE): UnaryOpUGen = Pure(this, a)

    def make1(a: Float): Float
  }

  sealed abstract class RandomOp extends Op {
    op =>

    final def make(a: GE): GE = makeNoOptimization(a)

    final def make1(a: UGenIn): UGenIn = UGenImpl(op, a, isIndividual = true, hasSideEffect = false)

    final def makeNoOptimization(a: GE): UnaryOpUGen = Random(this, a)
  }

  case object Neg extends PureOp {
    final val id = 0
    override val name = "-"
    override def prefix = true
    def make1(a: Float): Float = rf2.neg(a)
  }

  case object Not extends PureOp {
    final val id = 1
    override val name = "!"
    override def prefix = true
    def make1(a: Float): Float = rf2.not(a)
  }

  // case object IsNil       extends Op(  2 )
  // case object NotNil      extends Op(  3 )
  case object BitNot extends PureOp {
    final val id = 4
    def make1(a: Float): Float = (~a.toInt).toFloat
  }
  case object Abs extends PureOp {
    final val id = 5
    def make1(a: Float): Float = rf.abs(a)
  }

  // case object ToFloat     extends Op(  6 )
  // case object ToInt       extends Op(  7 )
  case object Ceil extends PureOp {
    final val id = 8
    def make1(a: Float): Float = rf.ceil(a)
  }

  case object Floor extends PureOp {
    final val id = 9
    def make1(a: Float): Float = rf.floor(a)
  }

  case object Frac extends PureOp {
    final val id = 10
    def make1(a: Float): Float = rf.frac(a)
  }

  case object Signum extends PureOp {
    final val id = 11
    def make1(a: Float): Float = rf.signum(a)
  }

  case object Squared extends PureOp {
    final val id = 12
    def make1(a: Float): Float = rf.squared(a)
  }

  case object Cubed extends PureOp {
    final val id = 13
    def make1(a: Float): Float = rf2.cubed(a)
  }

  case object Sqrt extends PureOp {
    final val id = 14
    def make1(a: Float): Float = rf.sqrt(a)
  }

  case object Exp extends PureOp {
    final val id = 15
    def make1(a: Float): Float = rf.exp(a)
  }

  case object Reciprocal extends PureOp {
    final val id = 16
    def make1(a: Float): Float = rf2.reciprocal(a)
  }

  // XXX TODO: Use proper camel-case naming in next major version
  case object Midicps extends PureOp {
    final val id = 17
    override val name = "midiCps"
    def make1(a: Float): Float = rf.midiCps(a)
  }

  case object Cpsmidi extends PureOp {
    final val id = 18
    override val name = "cpsMidi"
    def make1(a: Float): Float = rf.cpsMidi(a)
  }

  case object Midiratio extends PureOp {
    final val id = 19
    override val name = "midiRatio"
    def make1(a: Float): Float = rf.midiRatio(a)
  }

  case object Ratiomidi extends PureOp {
    final val id = 20
    override val name = "ratioMidi"
    def make1(a: Float): Float = rf.ratioMidi(a)
  }

  case object Dbamp extends PureOp {
    final val id = 21
    override val name = "dbAmp"
    def make1(a: Float): Float = rf.dbAmp(a)
  }

  case object Ampdb extends PureOp {
    final val id = 22
    override val name = "ampDb"
    def make1(a: Float): Float = rf.ampDb(a)
  }

  case object Octcps extends PureOp {
    final val id = 23
    override val name = "octCps"
    def make1(a: Float): Float = rf.octCps(a)
  }

  case object Cpsoct extends PureOp {
    final val id = 24
    override val name = "cpsOct"
    def make1(a: Float): Float = rf.cpsOct(a)
  }

  case object Log extends PureOp {
    final val id = 25
    def make1(a: Float): Float = rf.log(a)
  }

  case object Log2 extends PureOp {
    final val id = 26
    def make1(a: Float): Float = rf.log2(a)
  }

  case object Log10 extends PureOp {
    final val id = 27
    def make1(a: Float): Float = rf.log10(a)
  }

  case object Sin extends PureOp {
    final val id = 28
    def make1(a: Float): Float = rf.sin(a)
  }

  case object Cos extends PureOp {
    final val id = 29
    def make1(a: Float): Float = rf.cos(a)
  }

  case object Tan extends PureOp {
    final val id = 30
    def make1(a: Float): Float = rf.tan(a)
  }

  case object Asin extends PureOp {
    final val id = 31
    def make1(a: Float): Float = rf.asin(a)
  }

  case object Acos extends PureOp {
    final val id = 32
    def make1(a: Float): Float = rf.acos(a)
  }

  case object Atan extends PureOp {
    final val id = 33
    def make1(a: Float): Float = rf.atan(a)
  }

  case object Sinh extends PureOp {
    final val id = 34
    def make1(a: Float): Float = rf.sinh(a)
  }

  case object Cosh extends PureOp {
    final val id = 35
    def make1(a: Float): Float = rf.cosh(a)
  }

  case object Tanh extends PureOp {
    final val id = 36
    def make1(a: Float): Float = rf.tanh(a)
  }

  case object Rand extends RandomOp {
    final val id = 37
  }

  case object Rand2 extends RandomOp {
    final val id = 38
  }

  case object Linrand extends RandomOp {
    final val id = 39
    override val name = "linRand"
  }

  case object Bilinrand extends RandomOp {
    final val id = 40
    override val name = "bilinRand"
  }

  case object Sum3rand extends RandomOp {
    final val id = 41
    override val name = "sum3Rand"
  }

  case object Distort extends PureOp {
    final val id = 42
    def make1(a: Float): Float = rf2.distort(a)
  }

  case object Softclip extends PureOp {
    final val id = 43
    override val name = "softClip"
    def make1(a: Float): Float = rf2.softClip(a)
  }

  case object Coin extends RandomOp {
    final val id = 44
  }

  // case object DigitValue  extends Op( 45 )
  // case object Silence     extends Op( 46 )
  // case object Thru        extends Op( 47 )
   case object RectWindow extends PureOp {
    final val id = 48
    def make1(a: Float): Float = rf2.rectWindow(a)
  }
  case object HannWindow extends PureOp {
    final val id = 49
    def make1(a: Float): Float = rf2.hannWindow(a)
  }
  case object WelchWindow extends PureOp {
    final val id = 50
    def make1(a: Float): Float = rf2.welchWindow(a)
  }
  case object TriWindow extends PureOp {
    final val id = 51
    def make1(a: Float): Float = rf2.triWindow(a)
  }
  case object Ramp extends PureOp {
    final val id = 52
    def make1(a: Float): Float = rf2.ramp(a)
  }

  case object Scurve extends PureOp {
    final val id = 53
    override val name = "sCurve"
    def make1(a: Float): Float = rf2.sCurve(a)
  }

  override def read(in: RefMapIn, key: String, arity: Int): UnaryOpUGen = {
    require (arity == 2)
    val _op = in.readProductT[Op]()
    val _a  = in.readGE()
    apply(_op, _a)
  }

  private final case class Pure(selector: Op, a: GE)
    extends UnaryOpUGen

//  private final case class Impure(selector: Op, a: GE)
//    extends UnaryOpUGen with HasSideEffect {
//  }

  private final case class Random(selector: Op, a: GE)
    extends UnaryOpUGen with UsesRandSeed

  private def UGenImpl(selector: Op, a: UGenIn, isIndividual: Boolean, hasSideEffect: Boolean): UGen.SingleOut =
      UGen.SingleOut("UnaryOpUGen", a.rate, Vector(a),
        isIndividual = isIndividual, hasSideEffect = hasSideEffect, specialIndex = selector.id)
}

abstract class UnaryOpUGen extends UGenSource.SingleOut {

  def selector: UnaryOpUGen.Op
  def a: GE

  override final def productPrefix = "UnaryOpUGen"

  final def rate: MaybeRate = a.rate

  protected final def makeUGens: UGenInLike = unwrap(this, Vector(a.expand))

  protected final def makeUGen(args: Vec[UGenIn]): UGenInLike = {
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
object BinaryOpUGen extends ProductType[BinaryOpUGen] {
  // note: this is not optimizing, as would be `op.make(a, b)`, because it guarantees that the return
  // type is BinaryOpUGen. this is used in deserialization, you should prefer `op.make` instead.
  def apply(op: Op, a: GE, b: GE): BinaryOpUGen = op.makeNoOptimization(a, b)

  def unapply(b: BinaryOpUGen): Option[(Op, GE, GE)] = Some((b.selector, b.a, b.b))

  final val typeId = 444

  object Op extends ProductType[Op] {
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
      case Rrand    .id => Rrand
      case Exprand  .id => Exprand
    }

    final val typeId = 445

    override def read(in: RefMapIn, key: String, arity: Int): Op = {
      require (arity == 1)
      val _id = in.readInt()
      Op(_id)
    }
  }

  import UnaryOpUGen.{Neg, Reciprocal}

  private def uncapitalize(in: String): String = if (in.isEmpty) in else {
    val c0 = in.charAt(0)
    if (Character.isUpperCase(c0)) s"${Character.toLowerCase(c0)}${in.substring(1)}" else in
  }

  sealed abstract class Op extends Product {
    op =>

    def id: Int

    def make(a: GE, b: GE): GE

    protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn

    /** Whether the operator should be written in infix position.
      * For example, `Lcm` is '''not''' in prefix position, you would write
      * `a.lcm(b)`, whereas `Plus` is prefix position, as you would write
      * `a + b`.
      */
    def infix: Boolean = false

    /** The name one would call on `GEOps` to obtain this operator.
      * For example, for `Times` this would be `*`.
      */
    def methodName: String = name

    def makeNoOptimization(a: GE, b: GE): BinaryOpUGen

    override def productPrefix  = s"BinaryOpUGen$$Op"
    override def productArity   = 1
    override def productElement(n: Int): Any = if (n == 0) id else throw new IndexOutOfBoundsException(n.toString)

    def name: String = uncapitalize(plainName)

    private def plainName: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      cn.substring(i, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  sealed abstract class PureOp extends Op {
    def make(a: GE, b: GE): GE = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => Pure(this, a, b)
    }

    protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => UGenImpl(this, a, b, isIndividual = false, hasSideEffect = false)
    }

    def make1(a: Float, b: Float): Float

    final def makeNoOptimization(a: GE, b: GE): BinaryOpUGen = Pure(this, a, b)
  }

  sealed abstract class ImpureOp extends Op {
    final def makeNoOptimization(a: GE, b: GE): BinaryOpUGen = Impure(this, a, b)
  }

  sealed abstract class RandomOp extends Op {
    final def make(a: GE, b: GE): GE = makeNoOptimization(a, b)

    final protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn =
      UGenImpl(this, a, b, isIndividual = true, hasSideEffect = false)

    final def makeNoOptimization(a: GE, b: GE): BinaryOpUGen = Random(this, a, b)
  }

  case object Plus extends PureOp {
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

  case object Minus extends PureOp {
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

  case object Times extends PureOp {
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
  case object Div extends PureOp {
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

  case object Mod extends PureOp {
    final val id = 5
    override val name = "%"
    override def infix = true
    def make1(a: Float, b: Float): Float = rf.mod(a, b)
  }

  case object Eq extends PureOp {
    final val id = 6
    override val name = "sig_=="
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a == b) 1 else 0
  }

  case object Neq extends PureOp {
    final val id = 7
    override val name = "sig_!="
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a != b) 1 else 0
  }

  case object Lt extends PureOp {
    final val id = 8
    override val name = "<"
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a < b) 1f else 0f // NOT rf.< !
  }

  case object Gt extends PureOp {
    final val id = 9
    override val name = ">"
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a > b) 1f else 0f // NOT rf.> !
  }

  case object Leq extends PureOp {
    final val id = 10
    override val name = "<="
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a <= b) 1f else 0f // NOT rf.<= !
  }

  case object Geq extends PureOp {
    final val id = 11
    override val name = ">="
    override def infix = true
    def make1(a: Float, b: Float): Float = if (a >= b) 1f else 0f // NOT rf.>= !
  }

  case object Min extends PureOp {
    final val id = 12
    def make1(a: Float, b: Float): Float = rf.min(a, b)
  }

  case object Max extends PureOp {
    final val id = 13
    def make1(a: Float, b: Float): Float = rf.max(a, b)
  }

  case object BitAnd extends PureOp {
    final val id = 14
    override def infix = true
    override val name = "&"
    def make1(a: Float, b: Float): Float = rf.&(a, b)
  }

  case object BitOr extends PureOp {
    final val id = 15
    override def infix = true
    override val name = "|"
    def make1(a: Float, b: Float): Float = rf.|(a, b)
  }

  case object BitXor extends PureOp {
    final val id = 16
    override def infix = true
    override val name = "^"
    def make1(a: Float, b: Float): Float = rf.^(a, b)
  }

  case object Lcm extends PureOp {
    final val id = 17
    def make1(a: Float, b: Float): Float = lf.lcm(a.toLong, b.toLong).toFloat
  }

  case object Gcd extends PureOp {
    final val id = 18
    def make1(a: Float, b: Float): Float = lf.gcd(a.toLong, b.toLong).toFloat
  }

  case object RoundTo extends PureOp {
    final val id = 19
    def make1(a: Float, b: Float): Float = rf.roundTo(a, b)
  }

  case object RoundUpTo extends PureOp {
    final val id = 20
    def make1(a: Float, b: Float): Float = rf.roundUpTo(a, b)
  }

  case object Trunc extends PureOp {
    final val id = 21
    def make1(a: Float, b: Float): Float = rf.trunc(a, b)
  }

  case object Atan2 extends PureOp {
    final val id = 22
    def make1(a: Float, b: Float): Float = rf.atan2(a, b)
  }

  case object Hypot extends PureOp {
    final val id = 23
    def make1(a: Float, b: Float): Float = rf.hypot(a, b)
  }

  case object Hypotx extends PureOp {
    final val id = 24
    override val name = "hypotApx"
    def make1(a: Float, b: Float): Float = rf.hypotApx(a, b)
  }

  /** '''Warning:''' Unlike a normal power operation, the signum of the
    * left operand is always preserved. I.e. `DC.kr(-0.5).pow(2)` will
    * not output `0.25` but `-0.25`. This is to avoid problems with
    * floating point noise and negative input numbers, so
    * `DC.kr(-0.5).pow(2.001)` does not result in a `NaN`, for example.
    */
  case object Pow extends PureOp {
    final val id = 25
    def make1(a: Float, b: Float): Float = rf.pow(a, b)
  }

  case object LeftShift extends PureOp {
    final val id = 26
    override def infix = true
    override val name = "<<"
    def make1(a: Float, b: Float): Float = (a.toLong << b.toInt).toFloat
  }

  case object RightShift extends PureOp {
    final val id = 27
    override def infix = true
    override val name = ">>"
    def make1(a: Float, b: Float): Float = (a.toLong >> b.toInt).toFloat
  }
  // case object UnsgnRghtShft  extends Op( 28 )
  // case object Fill           extends Op( 29 )
  case object Ring1 extends PureOp {
    final val id = 30
    def make1(a: Float, b: Float): Float = rf2.ring1(a, b)
  }

  case object Ring2 extends PureOp {
    final val id = 31
    def make1(a: Float, b: Float): Float = rf2.ring2(a, b)
  }

  case object Ring3 extends PureOp {
    final val id = 32
    def make1(a: Float, b: Float): Float = rf2.ring3(a, b)
  }

  case object Ring4 extends PureOp {
    final val id = 33
    def make1(a: Float, b: Float): Float = rf2.ring4(a, b)
  }

  /*
   * Note: we do not use camel-case for the object name
   * because it would break serialization for older versions.
   */
  case object Difsqr extends PureOp {
    final val id = 34
    override val name = "difSqr"
    def make1(a: Float, b: Float): Float = rf.difSqr(a, b)
  }

  case object Sumsqr extends PureOp {
    final val id = 35
    override val name = "sumSqr"
    def make1(a: Float, b: Float): Float = rf.sumSqr(a, b)
  }

  case object Sqrsum extends PureOp {
    final val id = 36
    override val name = "sqrSum"
    def make1(a: Float, b: Float): Float = rf.sqrSum(a, b)
  }

  case object Sqrdif extends PureOp {
    final val id = 37
    override val name = "sqrDif"
    def make1(a: Float, b: Float): Float = rf.sqrDif(a, b)
  }

  case object Absdif extends PureOp {
    final val id = 38
    override val name = "absDif"
    def make1(a: Float, b: Float): Float = rf.absDif(a, b)
  }

  case object Thresh extends PureOp {
    final val id = 39
    def make1(a: Float, b: Float): Float = rf2.thresh(a, b)
  }

  case object Amclip extends PureOp {
    final val id = 40
    override val name = "amClip"
    def make1(a: Float, b: Float): Float = rf2.amClip(a, b)
  }

  case object Scaleneg extends PureOp {
    final val id = 41
    override val name = "scaleNeg"
    def make1(a: Float, b: Float): Float = rf2.scaleNeg(a, b)
  }

  case object Clip2 extends PureOp {
    final val id = 42
    def make1(a: Float, b: Float): Float = rf.clip2(a, b)
  }

  case object Excess extends PureOp {
    final val id = 43
    def make1(a: Float, b: Float): Float = rf.excess(a, b)
  }

  case object Fold2 extends PureOp {
    final val id = 44
    def make1(a: Float, b: Float): Float = rf.fold2(a, b)
  }

  case object Wrap2 extends PureOp {
    final val id = 45
    def make1(a: Float, b: Float): Float = rf.wrap2(a, b)
  }

  case object Firstarg extends ImpureOp {
    final val id = 46

    override val name = "firstArg"

    override def make(a: GE, b: GE): GE = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => Impure(this, a, b)
    }

    override protected[synth] def make1(a: UGenIn, b: UGenIn): UGenIn = (a, b) match {
      case (c(af), c(bf)) => c(make1(af, bf))
      case _              => UGenImpl(this, a, b, isIndividual = false, hasSideEffect = true)
    }

    def make1(a: Float, b: Float): Float = a
  }

  case object Rrand extends RandomOp {
    final val id = 47
    override val name = "rangeRand"
  }

  case object Exprand extends RandomOp {
    final val id = 48
    override val name = "expRand"
  }

  override def read(in: RefMapIn, key: String, arity: Int): BinaryOpUGen = {
    require (arity == 3)
    val _op = in.readProductT[Op]()
    val _a  = in.readGE()
    val _b  = in.readGE()
    apply(_op, _a, _b)
  }

  private final case class Pure(selector: Op, a: GE, b: GE)
    extends BinaryOpUGen

  private final case class Impure(selector: Op, a: GE, b: GE)
    extends BinaryOpUGen with HasSideEffect

  private final case class Random(selector: Op, a: GE, b: GE)
    extends BinaryOpUGen with UsesRandSeed

  private[this] def UGenImpl(selector: Op, a: UGenIn, b: UGenIn,
                             isIndividual: Boolean, hasSideEffect: Boolean): UGen.SingleOut =
      UGen.SingleOut("BinaryOpUGen", a.rate max b.rate, Vector(a, b),
        isIndividual = isIndividual, hasSideEffect = hasSideEffect, specialIndex = selector.id)
}

abstract class BinaryOpUGen extends UGenSource.SingleOut {
  def selector: BinaryOpUGen.Op
  def a: GE
  def b: GE

  override final def productPrefix = "BinaryOpUGen"

  final def rate: MaybeRate = MaybeRate.max_?(a.rate, b.rate)

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