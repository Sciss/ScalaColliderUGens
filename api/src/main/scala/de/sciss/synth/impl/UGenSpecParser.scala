/*
 *  UGenSpecParser.scala
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

import de.sciss.synth.UGenSpec.{ArgumentType, ArgumentValue}

import scala.collection.SeqLike
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.xml.NodeSeq

private[synth] object UGenSpecParser {
  // private def DEFAULT_VERIFY = true

  def parseAll(source: xml.InputSource, docs: Boolean, verify: Boolean): Map[String, UGenSpec] = {
    val root = xml.XML.load(source)
    val ugenNodes: NodeSeq = root \\ "ugen"
    ugenNodes.iterator.map { n =>
      val spec = parse(n, docs = docs, verify = verify)
      spec.name -> spec
    } .toMap
  }

  private val nodeAttrKeys = Set(
    "name", // required
    "reads-bus",  "reads-buf",  "reads-fft", "random", "indiv",
    "writes-bus", "writes-buf", "writes-fft", "side-effect",
    "done-flag" /* , "provided" */,
    "helper", "sourcecode", "elem"
  )

  private val nodeChildKeys = Set(
    "rate", "no-outputs", "output", "arg", "doc"
  )

  private val argAttrKeys = Set(
    "name", // required
    "type", "init", "default", "rate", "ugen-in", "pos", "variadic", "prepend-size"
  )

  private val outputAttrKeys = Set(
    "name", "type", "variadic"
  )

  private val rateAttrKeys = Set(
    "name"  // required
  )

  private val impliedRateAttrKeys = rateAttrKeys ++ Set(
    "method", "method-alias", "implied"
  )

  private val argRateAttrKeys = Set(
    "name", "default", "rate"
  )

  private implicit final class RichAttrMap(val map: Map[String, String]) extends AnyVal {
    def string (key: String): String = map.getOrElse(key, sys.error(s"Missing required attribute '$key'"))
    def boolean(key: String, default: Boolean = false): Boolean = map.get(key).map(_.toBoolean).getOrElse(default)
    def intOption(key: String): Option[Int] = map.get(key).map(_.toInt)
  }

  private object IsFloat {
    private val re = """\-?\d+\.\d+""".r.pattern
    def unapply(s: String): Option[Float] = {
      if (re.matcher(s).matches()) Some(s.toFloat) else None
    }
  }

  private object IsInt {
    private val re = """\-?\d+""".r.pattern
    def unapply(s: String): Option[Int] = {
      if (re.matcher(s).matches()) Some(s.toInt) else None
    }
  }

  private object IsBoolean {
    def unapply(s: String): Option[Boolean]= s match {
      case "true"   => Some(true)
      case "false"  => Some(false)
      case _        => None
    }
  }

  private object IsDoneAction {
    def unapply(s: String): Option[DoneAction] = PartialFunction.condOpt(s) {
      case doNothing        .name => doNothing
      case pauseSelf        .name => pauseSelf
      case freeSelf         .name => freeSelf
      case freeSelfPred     .name => freeSelfPred
      case freeSelfSucc     .name => freeSelfSucc
      case freeSelfPredAll  .name => freeSelfPredAll
      case freeSelfSuccAll  .name => freeSelfSuccAll
      case freeSelfToHead   .name => freeSelfToHead
      case freeSelfToTail   .name => freeSelfToTail
      case freeSelfPausePred.name => freeSelfPausePred
      case freeSelfPauseSucc.name => freeSelfPauseSucc
      case freeSelfPredDeep .name => freeSelfPredDeep
      case freeSelfSuccDeep .name => freeSelfSuccDeep
      case freeAllInGroup   .name => freeAllInGroup
      case freeGroup        .name => freeGroup
    }
  }

  private object IsGate {
    def unapply(s: String): Option[Boolean] = s match {
      case "open"   => Some(true)
      case "closed" => Some(false)
      case _        => None
    }
  }

  private object IsTrigger {
    def unapply(s: String): Option[Boolean] = s match {
      case "high"   => Some(true)
      case "low"    => Some(false)
      case _        => None
    }
  }

  private def incompatibleTypeDefault(uName: String, aName: String, tpe: Any, df: Any): Nothing =
    sys.error(s"Mismatch between argument type and default value for ugen $uName, argument $aName, type is $tpe, default is $df")

  private def errorScalarType(uName: String, aName: String, tpe: ArgumentType): Nothing =
    sys.error(s"Cannot use scalar declaration with arguments of type $tpe, in $uName, argument $aName")

  // poor man's type inference
  private def inferArgType(uName: String, aName: String,
                           aAttrs: Map[String, String]): (ArgumentType, Option[ArgumentValue]) = {
    import UGenSpec.{SignalShape => Sig, _}
    import ArgumentType.GE

    val aTypeExp0 = aAttrs.get("type").map {
      case "ge"       => GE(Sig.Generic)
      case "ge-int"   => GE(Sig.Int)
      case "ge-string"=> GE(Sig.String)
      case "bus"      => GE(Sig.Bus)
      case "buf"      => GE(Sig.Buffer)
      case "fft"      => GE(Sig.FFT)
      case "trig"     => GE(Sig.Trigger)
      case "switch"   => GE(Sig.Switch)
      case "gate"     => GE(Sig.Gate)
      case "mul"      => GE(Sig.Mul)
      case "action"   => GE(Sig.DoneAction)
      case "done-flag"=> GE(Sig.DoneFlag)
      case "int"      => ArgumentType.Int
      case other      => sys.error(s"Unsupported type for ugen $uName, argument $aName: $other")
    }

    val isInit    = aAttrs.boolean("init")
    val aTypeExp  = if (!isInit) aTypeExp0 else aTypeExp0 match {
      case Some(tpe @ GE(sig, _)) =>
        // disallow for Trigger, Gate, Mul, DoneFlag
        sig match {
          case Sig.Generic    =>
          case Sig.Int        =>
          case Sig.String     =>
          case Sig.Bus        =>
          case Sig.Buffer     =>
          case Sig.FFT        =>
          case Sig.Switch     =>
          case Sig.DoneAction =>
          case _ =>
            errorScalarType(uName, aName, tpe)
        }
        Some(GE(sig, scalar = true))

      case Some(tpe @ ArgumentType.Int) =>
        if (!aAttrs.boolean("ugen-in")) {
          errorScalarType(uName, aName, tpe)
        }
        aTypeExp0

      case _ => aTypeExp0
    }

    def orGeneric = aTypeExp.getOrElse(GE(Sig.Generic, scalar = isInit))

    aAttrs.get("default") match {
      case Some(default) =>
        val (tpe, df) = adjustDefault(uName, aName, aTypeExp, isInit, default)
        tpe -> Some(df)
      case _ =>
        orGeneric -> None
    }
  }

  private def adjustDefault(uName: String, aName: String,
                            aTypeExp: Option[UGenSpec.ArgumentType], isInit: Boolean,
                            default: String): (UGenSpec.ArgumentType, ArgumentValue) = {
    import ArgumentType.GE
    import UGenSpec.{SignalShape => Sig}

    def orGeneric = aTypeExp.getOrElse(GE(Sig.Generic, scalar = isInit))

    default match {
      case IsFloat(f)       =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Generic,_) =>
          case GE(Sig.Mul,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        orGeneric -> ArgumentValue.Float(f)

      case IsInt(i) =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Int,_) =>
          case ArgumentType.Int =>
          case other =>
            incompatibleTypeDefault(uName, aName, other, default)
        }
        orGeneric -> ArgumentValue.Int(i)

      case IsTrigger(b)     =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Trigger,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        val tpe = GE(Sig.Trigger)
        if (isInit) errorScalarType(uName, aName, tpe)
        tpe -> ArgumentValue.Boolean(value = b)

      case IsBoolean(b)     =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Switch,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        GE(Sig.Switch, scalar = isInit) -> ArgumentValue.Boolean(value = b)

      case IsGate(b) =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Gate,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        val tpe = GE(Sig.Gate)
        if (isInit) errorScalarType(uName, aName, tpe)
        tpe -> ArgumentValue.Boolean(value = b)

      case IsDoneAction(a) =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.DoneAction,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        GE(Sig.DoneAction, scalar = isInit) -> ArgumentValue.DoneAction(a)

      case "nyquist" =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Generic,_) =>
          case GE(Sig.Int,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        orGeneric -> ArgumentValue.Nyquist

      case "inf" =>
        // ---- check correctness ----
        aTypeExp.foreach {
          case GE(Sig.Generic,_) =>
          case GE(Sig.Int,_) =>
          case other => incompatibleTypeDefault(uName, aName, other, default)
        }
        orGeneric -> ArgumentValue.Inf

      case _ =>
        aTypeExp match {
          case Some(GE(Sig.String,_)) =>
          case _ =>
            sys.error(s"Unsupported default value for ugen $uName, argument $aName: $default")
        }
        GE(Sig.String, scalar = isInit) -> ArgumentValue.String(default)
    }
  }

  private val DEFAULT_OUTPUTS =
    Vector(UGenSpec.Output(name = None, shape = UGenSpec.SignalShape.Generic, variadic = None))

  private def trimWhile[A, Repr <: SeqLike[A, Repr]](xs: SeqLike[A, Repr])(pred: A => Boolean): Repr = {
    val left  = xs.dropWhile(pred)
    val idx   = left.lastIndexWhere(pred) // why there's no dropRightWhile?
    if (idx >= 0) left.dropRight(left.size - idx) else left
  }

  private def trimCode(codeText: String): List[String] = {
    val trim0: Vec[String] = codeText.linesIterator.toIndexedSeq
    val trim  = trimWhile(trim0)(_.trim().isEmpty)
    if (trim .isEmpty) return Nil

    val trimH   = trim.head
    val indent  = trimH.indexOf(trimH.trim())
    trim.iterator.map { ln =>
      val t = ln.trim()
      val i = ln.indexOf(t) - indent
      if (i <= 0) t else {
        val pad = " " * i
        s"$pad$t"
      }
    } .toList
  }

  private def trimDoc(docText: String): List[String] = {
    val trim0 = docText.linesIterator.map(_.trim).toIndexedSeq
    val trim  = trimWhile(trim0)(_.isEmpty)
    val b     = List.newBuilder[String]
    val sb    = new StringBuilder

    def flush(): Unit =
      if (sb.nonEmpty) {
        b += sb.toString
        sb.clear()
      }

    var inPre = false
    trim.foreach { line =>
      if (inPre) {
        if (sb.nonEmpty) sb.append('\n')
        sb.append(line)
        if (line == "}}}") inPre = false
      } else {
        if (line.isEmpty) flush() else {
          if (line == "{{{") {
            flush()
            inPre = true
          }
          if (sb.nonEmpty) sb.append(' ')
          sb.append(line)
        }
      }
    }
    flush()
    b.result()
  }

  private def mkDoc(node: xml.Node, argDocs: Map[String, List[String]],
                    outputDocs: Map[String, List[String]]): Option[UGenSpec.Doc] = {
    val docOpt = (node \ "doc").headOption
    docOpt.flatMap { dNode =>
      val dSees: List[String] = (dNode \ "see").iterator.map(_.text).toList
      import UGenSpec.Example
      val dEx: List[Example] = (dNode \ "example").iterator.map { n =>
        val a     = n.attributes.asAttrMap
        val name  = a.string("name")
        val code  = trimCode(n.text)
        val tpe   = a.getOrElse("type", "simple") match {
          case "simple" => Example.Simple
          case "full"   => Example.Full
          case other    => sys.error(s"Unsupported example type '$other'")
        }
        Example(name = name, code = code, tpe = tpe)
      } .toList
      val dAttr     = dNode.attributes.asAttrMap
      val dWarnPos  = dAttr.boolean("warn-pos")
      val dText     = (dNode \ "text").text
      val hasAny    = !dText.isEmpty || dSees.nonEmpty || dWarnPos || argDocs.nonEmpty || outputDocs.nonEmpty
      if (hasAny) {
        val dTextT  = trimDoc(dText)
        val doc     = UGenSpec.Doc(body = dTextT, args = argDocs, outputs = Map.empty, links = dSees,
          warnPos = dWarnPos, examples = dEx)
        Some(doc)
      } else {
        None
      }
    }
  }

  def parse(node: xml.Node, docs: Boolean, verify: Boolean): UGenSpec = {
    if (node.label != "ugen") throw new IllegalArgumentException(s"Not a 'ugen' node: $node")

    import UGenSpec.{SignalShape => Sig, _}

    val attrs = node.attributes.asAttrMap
    val uName = attrs.string("name")
    val eName = attrs.get   ("elem")

    if (verify) {
      val unknown = attrs.keySet -- nodeAttrKeys
      require(unknown.isEmpty, s"Unsupported ugen attributes in $uName: ${unknown.mkString(",")}")

      val unknownN = node.child.iterator.collect { case e: xml.Elem => e.label } .toSet[String] -- nodeChildKeys
      require(unknownN.isEmpty, s"Unsupported ugen child nodes in $uName: ${unknownN.mkString(",")}")
    }

    // ---- attributes ----

    val readsBus      = attrs.boolean("reads-bus")
    val readsBuffer   = attrs.boolean("reads-buf")
    val readsFFT      = attrs.boolean("reads-fft")
    val random        = attrs.boolean("random")

    val writesBus     = attrs.boolean("writes-bus")
    val writesBuffer  = attrs.boolean("writes-buf")
    val writesFFT     = attrs.boolean("writes-fft")

    val sideEffect    = attrs.boolean("side-effect")
    val doneFlag      = attrs.boolean("done-flag")
    val indiv         = attrs.boolean("indiv")

    val isHelper      = attrs.boolean("helper")
    val hasSourceCode = attrs.boolean("sourcecode")

    var uAttr         = Set.empty[Attribute]
    import Attribute._
    if (readsBus)       uAttr += ReadsBus
    if (readsBuffer)    uAttr += ReadsBuffer
    if (readsFFT)       uAttr += ReadsFFT
    if (random)         uAttr += UsesRandSeed
    if (indiv)          uAttr += IsIndividual

    if (writesBus)      uAttr += WritesBus
    if (writesBuffer)   uAttr += WritesBuffer
    if (writesFFT)      uAttr += WritesFFT
    if (sideEffect)     uAttr += HasSideEffect

    if (doneFlag)       uAttr += HasDoneFlag
    if (isHelper)       uAttr += IsHelper
    if (hasSourceCode)  uAttr += HasSourceCode

    // val indSideEffect = writesBus || writesBuffer || writesFFT
    // val indIndiv      = readsBus  || readsBuffer  || readsFFT || indSideEffect || random

    val aNodes = node \ "arg"

    // ---- rates ----

    var argRatesMap: Map[String, Map[Rate, (Option[String], Option[RateConstraint])]] = Map.empty withDefaultValue Map.empty

    def getRateConstraint(map: Map[String, String]): Option[RateConstraint] = map.get("rate").map {
      case "ugen"     => RateConstraint.SameAsUGen
      case "audio"    => RateConstraint.Fixed(audio)
      case "control"  => RateConstraint.Fixed(control)
    }

    def getRate(map: Map[String, String]): Rate = {
      map.getOrElse("name", sys.error(s"Missing rate name in ugen $uName")) match {
        case "audio"    => audio
        case "control"  => control
        case "scalar"   => scalar
        case "demand"   => demand
        case other      => sys.error(s"""Invalid rate in ugen $uName: $other""")
      }
    }

    def addArgRate(r: Rate, rNode: xml.Node): Unit =
      (rNode \ "arg").foreach { raNode =>
        val raAttr  = raNode.attributes.asAttrMap
        val raName  = raAttr.string("name")
        val raDef   = raAttr.get("default")
        val raCons  = getRateConstraint(raAttr)

        if (verify) {
          val unknown = raAttr -- argRateAttrKeys
          if (unknown.nonEmpty) {
            sys.error(s"Unsupported attribute in rate specific argument settings, ugen $uName, rate $r, argument $raName: ${unknown.mkString(",")}")
          }
        }

        if (raDef.isEmpty && raCons.isEmpty) {
          sys.error(s"Empty rate specific argument setting, ugen $uName, rate $r, argument $raName")
        }

        val oldMap  = argRatesMap(raName)
        val newMap  = oldMap + (r -> ((raDef, raCons)))
        argRatesMap += raName -> newMap
      }

    val rNodes = node \ "rate"
    if (rNodes.isEmpty && !isHelper) sys.error(s"No rates specified for $uName")
    val impliedRate = (rNodes.size == 1) && rNodes.head.attributes.asAttrMap.boolean("implied")
    val rates = if (impliedRate) {
      val rNode   = rNodes.head
      val rAttr   = rNode.attributes.asAttrMap
      val r       = getRate(rAttr)

      if (verify) {
        val unknown = rAttr -- impliedRateAttrKeys
        if (unknown.nonEmpty)
          sys.error(s"Unsupported ugen rate attributes, in ugen $uName, rate $r: ${unknown.mkString(",")}")
      }

      val rMethod = (rAttr.get("method"), rAttr.get("method-alias")) match {
        case (None, None)     => RateMethod.Default
        //        case (Some(""), None) =>
        //          if (aNodes.nonEmpty || indIndiv)
        //            sys.error(s"Cannot produce singleton for ugen $uName that has arguments or is individual")
        //          RateMethod.None
        case (Some(m), None)  => RateMethod.Custom(m)
        case (None, Some(m))  => RateMethod.Alias(m)
        case _ =>
          sys.error(s"Cannot use both method and method-alias attributes, in ugen $uName, rate $r")
      }
      addArgRate(r, rNode)
      Rates.Implied(r, rMethod)

    } else {
      val set: Set[Rate] = rNodes.iterator.map { rNode =>
        val rAttr   = rNode.attributes.asAttrMap
        val r       = getRate(rAttr)

        if (verify) {
          val unknown = rAttr -- rateAttrKeys
          if (unknown.nonEmpty)
            sys.error(s"Unsupported ugen rate attributes, in ugen $uName, rate $r: ${unknown.mkString(",")}")
        }
        addArgRate(r, rNode)
        r
      } .toSet

      Rates.Set(set)
    }

    // ---- arguments ----

    var args          = Vector.empty: Vec[Argument]
    var argsOrd       = Map.empty[Int, Argument]
    var argMap        = Map.empty[String, Argument]
    var inputs        = Vector.empty[Input]
    var inputMap      = Map.empty[String, Input]
    var argDocs       = Map.empty[String, List[String]]

    aNodes.foreach { aNode =>
      val aAttrs      = aNode.attributes.asAttrMap
      val aName       = aAttrs.string("name")

      if (verify) {
        // verify attribute names
        val unknown = aAttrs.keySet -- argAttrKeys
        require(unknown.isEmpty, s"Unsupported argument attributes, in ugen $uName, $aName: ${unknown.mkString(",")}")

        // verify that no accidental text is in node (a typical error is omitting the <doc> node)
        require(aNode.child.collect { case t: xml.Text if t.text.trim.nonEmpty => t } .isEmpty,
          s"UGen $uName argument $aName contains text")
      }

      // have to deal with:
      // "type" (ok), "init" (ok), "default" (ok), "rate", "ugen-in" (ok), "pos" (ok), "variadic" (ok)
      val (aType, aDefaultOpt) = inferArgType(uName = uName, aName = aName, aAttrs)   // handles "type", "default", "init"

      val isInput = aType match {
        case ArgumentType.GE(_, _) => true
        case ArgumentType.Int => aAttrs.boolean("ugen-in")                             // handles "ugen-in"
      }

      if (isInput) {
        val variadic  = aAttrs.boolean("variadic")                                     // handles "variadic"
        val preSz     = aAttrs.boolean("prepend-size")
        val tpe = if (variadic) Input.Variadic(prependSize = preSz) else Input.Single
        val in  = Input(aName, tpe = tpe)
        inputMap += aName -> in
        inputs :+= in
      }

      var aDefaults   = Map.empty[MaybeRate, ArgumentValue]
      aDefaultOpt.foreach { df => aDefaults += UndefinedRate -> df }
      var aRates      = Map.empty[MaybeRate, RateConstraint]

      // constraints and rate specific defaults

      getRateConstraint(aAttrs).foreach(aRates += UndefinedRate -> _)

      argRatesMap(aName).foreach { case (r, (raDef, raCons)) =>
        raCons.foreach { cons =>
          aRates += r -> cons
        }
        raDef.foreach { df =>
          val isInit  = aType match {
            case ArgumentType.GE(_, b) => b
            case _ => false
          }
          val (_, dfv) = adjustDefault(uName, aName, Some(aType), isInit = isInit, default = df)
          aDefaults += r -> dfv
        }
      }

      def errorMixPos(): Nothing =
        sys.error(s"Cannot mix positional and non-positional arguments, in ugen $uName, argument $aName")

      val arg = Argument(aName, aType, aDefaults, aRates)
      aAttrs.intOption("pos") match {                                                 // handles "pos"
        case Some(pos) =>
          if (args.nonEmpty) errorMixPos()
          if (argsOrd.contains(pos)) {
            sys.error(s"Duplicate argument input position, in ugen $uName, argument $aName")
          }
          argsOrd += pos -> arg

        case _ =>
          if (argsOrd.nonEmpty) errorMixPos()
          args :+= arg
      }
      argMap += aName -> arg

      if (docs) {
        val aDoc  = (aNode \ "doc").text
        if (!aDoc.isEmpty) {
          val aDocT = trimDoc(aDoc)
          if (aDocT.nonEmpty) argDocs += aName -> aDocT
        }
      }
    }

    if (argsOrd.nonEmpty) {
      val sq = argsOrd.keys.toIndexedSeq.sorted
      if (sq != sq.indices) sys.error(s"Non contiguous argument positions, in ugen $uName: ${sq.mkString(",")}")
      args = sq.map(argsOrd)
    }

    if (verify) {
      val hasV = inputs.filter(_.variadic)
      if (hasV.nonEmpty) {
        if (hasV.size > 1) sys.error(s"Can only have one variadic argument, in ugen $uName: ${hasV.map(_.arg).mkString(",")}")
        if (hasV.head != inputs.last) sys.error(s"Variadic input must come last, in ugen $uName, argument ${hasV.head.arg}")
      }
    }

    // ---- outputs ----
    var outputs     = Vector.empty[Output]
    var outputDocs  = Map.empty[String, List[String]]

    val oNodes  =  node \ "output"
    val zeroOut = (node \ "no-outputs").nonEmpty
    if (zeroOut && oNodes.nonEmpty) sys.error(s"Cannot mix no-outputs and output nodes, in ugen $uName")

    if (!zeroOut && oNodes.isEmpty) {
      outputs = DEFAULT_OUTPUTS
    } else oNodes.foreach { oNode =>
      val oAttrs = oNode.attributes.asAttrMap
      if (verify) {
        val unknown = oAttrs.keySet -- outputAttrKeys
        require(unknown.isEmpty, s"Unsupported ugen outputs attributes: ${unknown.mkString(",")}")
      }

      val oName   = oAttrs.get("name")
      val oShape0 = oAttrs.get("type").map {
        case "ge"       => Sig.Generic
        case "ge-int"   => Sig.Int
        case "bus"      => Sig.Bus
        case "buf"      => Sig.Buffer
        case "fft"      => Sig.FFT
        case "trig"     => Sig.Trigger
        case "switch"   => Sig.Switch
        case "gate"     => Sig.Gate
        case other      =>
          sys.error(s"Unsupported type, in ugen $uName, output $oName: $other")
      }
      val oShape  = oShape0 getOrElse Sig.Generic

      val variadic  = oAttrs.get("variadic")
      variadic.foreach { id =>
        if (!argMap.contains(id))
          sys.error(s"Variadic output refers to unknown argument, in ugen $uName, output $oName, ref $id")
      }

      if (variadic.isDefined) oShape match {
        case Sig.Generic  =>
        case Sig.Int      =>
        case Sig.Trigger  =>
        case Sig.Switch   =>
        case Sig.Gate     =>
        case other        =>
          sys.error(s"Variadic output has unsupported type, in ugen $uName, output $oName: $other")
      }

      val out = Output(oName, shape = oShape, variadic = variadic)
      outputs :+= out

      if (docs) {
        val oDoc  = (oNode \ "doc").text
        if (!oDoc.isEmpty) {
          oName match {
            case Some(n) =>
              val oDocT = trimDoc(oDoc)
              if (oDocT .nonEmpty) outputDocs += n -> oDocT
            case _ =>
              sys.error(s"Documented outputs must be named, in $uName")
          }
        }
      }
    }

    // ---- wrap up ----

    val uDoc = if (docs) mkDoc(node, argDocs = argDocs, outputDocs = outputDocs) else None

    UGenSpec(name = uName, attr = uAttr, rates = rates, args = args, inputs = inputs, outputs = outputs,
      doc = uDoc, elemOption = eName)
  }
}