/*
 *  UGenSpec.scala
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

import de.sciss.synth.impl.{UGenSpecParser => ParserImpl}
import de.sciss.synth

import scala.collection.immutable
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}

object UGenSpec {
  /** List of standard UGen plugin names, including ScalaCollider helper elements. */
  final val standardPlugins: List[String] = List(
    "BinaryOpUGens", "ChaosUGens", "DelayUGens", "DemandUGens", "DiskIOUGens", "DynNoiseUGens", "FFT2_UGens",
    "FFT_UGens", "FilterUGens", "GendynUGens", "GrainUGens", "IOUGens", "KeyboardUGens", "LFUGens", "MachineListening",
    "MouseUGens", "MulAddUGens", "NoiseUGens", "OSCUGens", "PanUGens", "PhysicalModellingUGens", "ReverbUGens",
    "TestUGens", "TriggerUGens", "UnpackFFTUGens", "UnaryOpUGens",
    "HelperElements",
  )

  /** List of third-party UGens as per https://github.com/supercollider/sc3-plugins,
    * This is currently incomplete.
    */
  final val thirdPartyPlugins: List[String] = List(
    "DEINDUGens", "MCLDBufferUGens", "MCLDDistortionUGens", "MdaUGens", "TJUGens", "VBAPUGens", "AlmatLDUGens"
  )

  /** Lazily computes the specs of the UGens bundled with the standard SuperCollider distribution.
    * The result maps from UGen names to their specifications.
    */
  lazy val standardUGens: Map[String, UGenSpec] = mkUGens(standardPlugins)

  /** Lazily computes the specs of the UGens found in the sc3-plugins project.
    * The result maps from UGen names to their specifications.
    */
  lazy val thirdPartyUGens: Map[String, UGenSpec] = mkUGens(thirdPartyPlugins)

  private def mkUGens(names: List[String]): Map[String, UGenSpec] = names.iterator.flatMap { name =>
    val is = ugen.Control.getClass.getResourceAsStream(s"$name.xml")
    if (is == null) throw new Exception(s"UGenSpec.mkUGens - resource '$name.xml' not found.")
    try {
      val source = scala.xml.Source.fromInputStream(is)
      parseAll(source, docs = true)
    } finally {
      is.close()
    }
  } .toMap

  /** Parses a complete XML file containing a number of UGen specifications.
    *
    * @param source   the XML source, such as obtained from a file or input stream.
    * @param docs     if `true`, parses documentation as well. if `false` skips documentation, thus
    *                 `spec.doc` will be `None`.
    * @return         a map from UGen names to their specifications.
    */
  def parseAll(source: scala.xml.InputSource, docs: Boolean = false, verify: Boolean = false): Map[String, UGenSpec] =
    ParserImpl.parseAll(source, docs = docs, verify = verify)

  /** Parses an individual XML node for one specific UGen specification.
    *
    * @param node the `<ugen>` node.
    * @param docs if `true`, parses documentation as well. if `false` skips documentation, thus
    *             `spec.doc` will be `None`.
    */
  def parse(node: scala.xml.Node, docs: Boolean = false, verify: Boolean = false): UGenSpec =
    ParserImpl.parse(node, docs = docs, verify = verify)

  // ---- UGen attributes ----

  object Attribute {

    /** Some attributes imply side effects. For example if the UGen writes to a buffer, it cannot
      * be eliminated even if unconnected to any other UGen, thus performs a side effect.
      */
    sealed trait ImpliesSideEffect extends Attribute

    /** Some attributes imply that the UGen is individual, if it not performing side effects.
      * Individuality means that two instances of a UGen, even when having the same arguments,
      * cannot be reduced to one. An example is reading buffers. If two UGens A and B
      * read from the same buffer, they could still be at different positions within the UGen
      * graph, with another UGen between them which writes to that buffer; hence they could
      * in fact see two different signals.
      */
    sealed trait ImpliesIndividual extends Attribute

    /** Indicates that the UGen reads from an audio bus. */
    case object ReadsBus      extends Attribute.ImpliesIndividual   // cf Specified.txt
    /** Indicates that the UGen reads from a buffer. */
    case object ReadsBuffer   extends Attribute.ImpliesIndividual   // cf Specified.txt
    /** Indicates that the UGen reads from an FFT buffer. */
    case object ReadsFFT      extends Attribute.ImpliesIndividual   // cf Specified.txt
    /** Indicates that the UGen accesses a random number generator. */
    case object UsesRandSeed  extends Attribute.ImpliesIndividual
    /** Indicates that the UGen is otherwise individual. This is currently
      * used for many demand rate UGens which are internally advanced when polled,
      * so employing two times a `Dseq` with the same parameters should not collapse the UGens.
      */
    case object IsIndividual  extends Attribute.ImpliesIndividual

    /** Indicates that the UGen writes to an audio bus. */
    case object WritesBus     extends Attribute.ImpliesSideEffect with Attribute.ImpliesIndividual
    /** Indicates that the UGen writes to a buffer. */
    case object WritesBuffer  extends Attribute.ImpliesSideEffect with Attribute.ImpliesIndividual
    /** Indicates that the UGen writes to an FFT buffer. */
    case object WritesFFT     extends Attribute.ImpliesSideEffect with Attribute.ImpliesIndividual
    /** Indicates that the UGen has another kind of side effect. An example is
      * `DetectSilence` which executes a done action.
      */
    case object HasSideEffect extends Attribute.ImpliesSideEffect

    /** Indicates that the UGen sets the so-called "done-flag". This may be read by another UGen
      * which takes this UGen as input.
      */
    case object HasDoneFlag extends Attribute

    /** Indicates that this is not a genuine UGen, but a helper graph element provided by
      * ScalaCollider.
      */
    case object IsHelper extends Attribute

    /** Indicates that this is might be optimized at runtime to other UGens than the nominal UGen.
      * In this case, it is possible that the number of specified rates is zero.
      */
    case object IsOptimized extends Attribute

    /** Indicates that this UGen is not fully specified on the client-side. The client should
      * not make any specific assumptions about its workings, or assume that the specification
      * is complete (for example, some arguments might be omitted that cannot be represented).
      */
    case object IsFragment extends Attribute

    /** Indicates that manual source code is provided for this UGen. */
    case object HasSourceCode extends Attribute
  }
  /** An attribute describes an aspect of a UGen related to how it consumes resources, whether it is individual etc. */
  sealed trait Attribute

  // ---- UGen input arguments ----

  /** A UGen client-side (logical) input argument.
    *
    * @param name     the name of the argument. On the server-side UGen inputs do not have argument names, so this
    *                 is purely for the user-interface. It will be the argument name of the constructor arguments
    *                 of the UGen class.
    * @param tpe      the argument type, such as graph element `GE` or integer.
    * @param defaults default values for the argument, as specified depending on the UGen's calculation rate.
    * @param rates    constraints for the argument's own rate, as specified depending on this UGen's calculation rate.
    */
  final case class Argument(name: String, tpe: ArgumentType,
                            defaults: Map[MaybeRate, ArgumentValue],
                            rates   : Map[MaybeRate, RateConstraint]) {
    override def toString: String = {
      val base = s"$name: $tpe"
      val s1 = defaults.get(UndefinedRate) match {
        case Some(v)  => s"$base = $v"
        case _        => base
      }
      val md = defaults - UndefinedRate
      val s2 = if (md.isEmpty) s1 else {
        s"$s1 ${md.mkString("[", ", ", "]")}"
      }
      val s3 = rates.get(UndefinedRate) match {
        case Some(v)  => s"$s2 @$v"
        case _        => s2
      }
      val mr = rates - UndefinedRate
      if (mr.isEmpty) s3 else {
        s"$s3 -> ${mr.mkString("[", ", ", "]")}"
      }
    }
  }

  object ArgumentType {
    /** An integer input. This is typically used to determine the UGen's number of channels. */
    case object Int extends ArgumentType

    /** A graph element input. This is further defined by a signal shape, and a boolean which indicates
      * whether the input is evaluated only at the UGen's init phase.
      *
      * @param shape    the shape of signal. This may influence the choice of client-side Scala argument type
      *                 used. For example `SignalShape.String` means that the argument will indeed become
      *                 `String` instead of `GE`, although in the UGen expansion that string is converted
      *                 to UGen inputs.
      * @param scalar   if `true`, the input will be evaluated by the UGen only in its initialization phase.
      *                 It is still legal to pass in non-scalar input elements, this is merely an information
      *                 usable by the client.
      */
    final case class GE(shape: SignalShape, scalar: Boolean = false) extends ArgumentType {
      override def toString: String = {
        val base = shape.toString
        if (scalar) s"$base (@init)" else base
      }
    }
  }
  /** Currently, two types of arguments are supported for UGen classes: `GE` and `Int`.
    * However, the actual Scala type for graph elements may differ, depending on the GE's signal shape.
    * If that shape is `String`, the argument may appear to have type `String` which is then internally
    * converted to a UGen input.
    */
  sealed trait ArgumentType

  object RateConstraint {
    /** The rate of a UGen's argument must be the same as the UGen's own calculation rate. */
    case object SameAsUGen extends RateConstraint {
      override def toString = "same-rate-as-ugen"
    }
    /** The rate of a UGen's argument must be exactly as specified here. */
    final case class Fixed(rate: Rate) extends RateConstraint {
      override def toString = s"fixed-rate=$rate"
    }
  }
  /** One of a fixed set of constraints on a UGen argument's calculation rate. */
  sealed trait RateConstraint

  object SignalShape {
    /** A generic quasi-continuous (float) control or audible signal. */
    case object Generic     extends SignalShape
    /** A signal which is used as an integer, for example to distinguish a discrete set of values. */
    case object Int         extends SignalShape
    /** A signal which is decoded as a character string. */
    case object String      extends SignalShape
    /** A signal which indicates a bus index. */
    case object Bus         extends SignalShape
    /** A signal which indicates an audio buffer identifier. */
    case object Buffer      extends SignalShape
    /** A signal which indicates an FFT buffer identifier. */
    case object FFT         extends SignalShape
    /** A signal which acts as a trigger. A trigger occurs when a signal crosses
      * from non-positive to positive.
      *
      * In the XML specification this shape is inferred from values `low` and `high`.
      */
    case object Trigger     extends SignalShape

    /** A signal which acts as a binary on-off switch. A switch is off when zero,
      * and on when greater than zero.
      *
      * In the XML specification this shape is inferred from values `false` and `true`.
      */

    case object Switch      extends SignalShape
    /** A signal which acts as a binary closed-open gate. A gate is closed when zero,
      * and open when greater than zero.
      *
      * In the XML specification this shape is inferred from values `closed` and `open`.
      */
    case object Gate        extends SignalShape
    /** A signal which acts as a multiplier input. */
    case object Mul         extends SignalShape
    /** A signal which represents one of the predefined done-action values. */
    case object DoneAction  extends SignalShape
    /** An out signal which is read only in terms of its special done-flag. */
    case object DoneFlag    extends SignalShape
  }
  /** The logical shape or type or "meaning" of a signal. */
  sealed trait SignalShape

  object ArgumentValue {
    /** Value is given as an `Int` constant. */
    final case class Int(value: scala.Int) extends ArgumentValue {
      override def toString: Predef.String = value.toString

      def toGE: ugen.Constant = ugen.Constant(value)
    }
    /** Value is given as a `Float` constant. */
    final case class Float(value: scala.Float) extends ArgumentValue {
      override def toString: Predef.String = {
        val s = value.toString
        if (s.contains('.')) s else s"$s.0"
      }

      def toGE: ugen.Constant = ugen.Constant(value)
    }
    /** Value is given as a `Boolean` constant. Currently the class synthesizer
      * does not support this, and automatically uses `0` and `1`.
      */
    final case class Boolean(value: scala.Boolean) extends ArgumentValue {
      override def toString: Predef.String = value.toString

      def toGE: ugen.Constant = ugen.Constant(if (value) 1f else 0f)
    }
    /** Value is a `String` literal. */
    final case class String(value: java.lang.String) extends ArgumentValue {
      override def toString = s""""$value""""

      def toGE: GE = UGenSource.stringArg(value)
    }
    /** Value is `Float.PositiveInfinity` (but more prettily written). */
    case object Inf extends ArgumentValue {
      override def toString: Predef.String = productPrefix.toLowerCase

      def toGE: ugen.Constant = ugen.Constant(scala.Float.PositiveInfinity)
    }
    /** Values is a `DoneAction`, such as `doNothing` or `freeSelf`. */
    final case class DoneAction(peer: synth.DoneAction) extends ArgumentValue {
      override def toString: Predef.String = peer.toString

      def toGE: ugen.Constant = synth.DoneAction.toGE(peer)
    }
    /** Value indicates Nyquist frequency. This is mapped to a method expanding to `SampleRate.ir / 2`. */
    case object Nyquist extends ArgumentValue {
      override def toString = s"$productPrefix()"

      def toGE: GE = ugen.Nyquist()
    }
  }
  /** Type of default value for a UGen constructor argument. This allows for the inclusion
    * of special values such as `Nyquist` or done actions that are more specific than
    * for example `Constant`.
    */
  sealed trait ArgumentValue {
    def toGE: GE
  }

  object Input {
    sealed trait Type { def variadic: Boolean }
    case object Single extends Type { def variadic = false }
    case class Variadic(prependSize: Boolean) extends Type { def variadic = true }
  }
  /** A UGen (server-side) input corresponds with a particular argument in the client-side interface.
    *
    * @param arg    the name of the argument (in the spec's `args` sequence) corresponding to the UGen input.
    * @param tpe    if variadic, the signal is treated as a multi-channel input with variable number of channels.
    *               only the last input of a UGen can be variadic. An example is the second input of
    *               the `Out` UGen.
    */
  final case class Input(arg: String, tpe: Input.Type) {
    override def toString: String = if (tpe.variadic) s"$arg..." else arg

    def variadic: Boolean = tpe.variadic
  }

  // ---- Supported rates ----

  object RateMethod {
    /** The default method name for a given rate. That is, `ar` for audio-rate, `kr` for control-rate,
      * `ir` for scalar-rate, and `dr` for demand-rate.
      */
    case object Default extends RateMethod
    /** Uses a custom method name for the given implied rate.
      * Often the name will be `"apply"`, for example with the FFT UGens
      * (`PV_MagAbove` etc.). Then the UGen can be constructed as `PV_MagAbove(...)` instead of
      * `PV_MagAbove.kr(...)`.
      */
    case class Custom(name: String) extends RateMethod

    /** Allows for a secondary method to be generated next to the default method. That is, it
      * acts as a combination of `Default` and `Custom`. This is currently not used for the
      * standard UGens.
      */
    case class Alias (name: String) extends RateMethod
    //    /** OBSOLETE: This value is not used any more. */
    //    case object None extends RateMethod
  }
  /** The shape of a UGen's constructor method relating to a particular calculation rate.
    * For example, it may be one of the standard namings like `ar` for audio-rate
    * (`RateMethod.Default`), or it may be a custom method.
    */
  sealed trait RateMethod

  object Rates {
    /** An implied rate means that the UGen can only possibly run at one particular rate.
      * When an implied rate is used, the UGen mixes in a rate trait, such as `AudioRated` and
      * does not include an explicit `rate` argument in its case class constructor.
      *
      * @param rate     the rate at which the UGen runs
      * @param method   the type of constructor method to generate
      */
    final case class Implied(rate: Rate, method: RateMethod) extends Rates {
      def set: ISet[Rate] = ISet(rate)

      override def toString: String = {
        val base = s"implied: $rate"
        method match {
          case RateMethod.Default => base
          case _                  => s"$base (method = $method)"
        }
      }

      def methodName(r: Rate): String = {
        require(r == rate)
        method match {
          case RateMethod.Default       => r.methodName
          case RateMethod.Custom(name)  => name
          case RateMethod.Alias(name)   => name
        }
      }
    }
    /** An explicit set of supported rates. */
    final case class Set(set: immutable.Set[Rate]) extends Rates {
      override def toString: String = set.mkString("[", ", ", "]")
      /** Explicitly specified rates always use the `Default` type of method naming. */
      def method: RateMethod = RateMethod.Default

      def methodName(r: Rate): String = {
        require(set.contains(r))
        r.methodName
      }
    }
  }
  /** The supported calculation rates of a UGen can be either implied or a specified set of rates. */
  sealed trait Rates {
    def method: RateMethod
    /** The set of supported rates, whether implied or explicit. */
    def set: Set[Rate]

    def methodName(r: Rate): String
  }

  // ---- Outputs ----

  //  object Outputs {
  //    final case class Argument(name: String) extends Outputs
  //    // type = fft
  //
  //  }
  //  sealed trait Outputs

  /** Specification of a UGen output. Note that this describes one logical output. A UGen with a
    * variable number of output channels can be seen has having _one_ logical output. For instance,
    * the `In` UGen has one output signal, even if that signal may have any number of channels.
    * In contrast, the `Pitch` UGen has two discrete logical outputs, one indicating the status
    * ("hasFreq"), the other giving the actually seen frequency.
    *
    * @param name     logical name of the output. The server does not know about output names,
    *                 this is purely for client side usage (e.g. in a GUI or help system).
    * @param shape    the "shape" or type of output signal, for example (continuous) generic or binary trigger.
    *                 technically all signals are the same, the shape just indicates the expected shape of that
    *                 signal.
    * @param variadic if defined, specifies the name of an argument (in the spec's `args`) which must be of
    *                 type `Int`, determining the number of channels in this output.
    */
  final case class Output(name: Option[String], shape: SignalShape, variadic: Option[String]) {
    override def toString: String = {
      val base  = name getOrElse "<out>"
      val s1    = if (shape != SignalShape.Generic) s"$base: $shape" else base
      variadic match {
        case Some(id) => s"$s1... ($id)"
        case _ => s1
      }
    }
  }

  object Example {
    sealed trait Type
    /** Example specifies UGens which can be wrapped in a `play { ... }` block */
    case object Simple extends Type
    /** A full scale example which should be executed by itself without wrapping. */
    case object Full   extends Type
  }
  final case class Example(name: String, code: List[String], tpe: Example.Type)

  /** Documentation of a UGen.
    *
    * @param body     the main UGen description text, as a list of paragraphs.
    * @param args     maps argument names to documentation for that argument, given as a list of paragraphs.
    * @param outputs  maps output names to documentation for that output, given as a list of paragraphs.
    * @param links    list of cross-links to related UGens.
    * @param warnPos  if `true`, indicates that the argument positions are different than in SC-Lang,
    *                 warranting an explicit warning to the user to raise awareness.
    */
  final case class Doc(body: List[String], args: Map[String, List[String]], outputs: Map[String, List[String]],
                       links: List[String], warnPos: Boolean, examples: List[Example])
}

/** Specification of a Unit Generator.
  *
  * @param name       the name of the unit generator, as seen by the server
  * @param attr       a set of attributes which characterize the UGen, such as resource usage or uniqueness
  * @param rates      at which calculation rates the UGen runs, and whether the rate is implied
  * @param args       the constructor arguments of the UGen representation. This is the interface for the client-side
  *                   instantiation and may include types other than `GE`, for example integers for fixed
  *                   number of channels, etc., as well as definitions for default values.
  * @param inputs     the inputs as passed to the underlying UGen (server-side object). Inputs are things which
  *                   expand to `UGenInLike` elements. Typically they correspond to client-side `args` arguments.
  *                   The order of this sequence must be correctly reflecting the UGen plugin interface, whereas
  *                   the order of the `args` sequence may diverge for an improved user interface.
  * @param outputs    a list of output specifications
  * @param doc        optional text documentation
  * @param elemOption diverging name of the graph element, as seen by the client
  */
final case class UGenSpec(name      : String,
                          attr      : Set[UGenSpec.Attribute],
                          rates     : UGenSpec.Rates,
                          args      : Vec[UGenSpec.Argument],
                          inputs    : Vec[UGenSpec.Input  ],
                          outputs   : Vec[UGenSpec.Output ],
                          doc       : Option[UGenSpec.Doc],
                          elemOption: Option[String]
                         ) {
  /** A convenience field which maps from argument names to arguments. */
  lazy val argMap:   Map[String, UGenSpec.Argument] = args  .iterator.map(a => a.name -> a).toMap
  /** A convenience field which maps from input argument names to inputs. */
  lazy val inputMap: Map[String, UGenSpec.Input   ] = inputs.iterator.map(i => i.arg  -> i).toMap

  def className: String = elemOption.getOrElse(name)

  override def toString: String = {
    val s1 = s"$name, attr = ${attr.mkString("[", ", ", "]")}, rates = $rates, "
    val s2 = s"args = ${       args.mkString("[", ", ", "]")}, inputs = ${inputs.mkString("[", ", ", "]")}, "
    val s3 = s"outputs = ${ outputs.mkString("[", ", ", "]")}"
    val s4 = if (elemOption.isEmpty) "" else s", elemOption = $elemOption"
    s"$productPrefix($s1$s2$s3$s4)"
  }
}