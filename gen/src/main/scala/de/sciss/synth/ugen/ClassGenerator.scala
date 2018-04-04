/*
 *  ClassGenerator.scala
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

import java.io.FileOutputStream

import de.sciss.file._
import de.sciss.synth.UGenSpec.ArgumentType.GE
import de.sciss.synth.UGenSpec.Attribute.HasSourceCode
import de.sciss.synth.UGenSpec.{SignalShape => Sig, _}

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.io.Source
import scala.util.control.NonFatal

final class ClassGenerator {
  me =>
  
  val CHARSET       = "UTF-8"

  val DocWidth      =  80
  val LineWidth     = 100
  val ParamColumns  =  24

  private[this] val forceCompanion = true

  def performFile(node: xml.Node, dir: File, name: String, docs: Boolean = true,
                  forceOverwrite: Boolean = false): Unit = try {
    val revision    = (node \ "@revision").text.toInt
    val thirdParty  = {
      val txt = (node \ "@third-party").text
      if (txt.isEmpty) None else Some(txt)
    }
    val fName       = s"$name.scala"
    val f           = new File(dir, fName)
    val write       = forceOverwrite || !f.isFile || {
      val source    = Source.fromFile(f, CHARSET)
      try {
        val it = source.getLines()
        !it.hasNext || {
          val line = it.next()
          val i = line.indexOf("revision: ")
          i < 0 || (line.substring(i + 10).toInt < revision)
        }
      } catch {
        case _: NumberFormatException => true
      } finally {
        source.close()
      }
    }
    val hasFile = !write || {
      val specs0 = (node \ "ugen") map { uNode =>
        UGenSpec.parse(uNode, docs = docs, verify = true)
      }
      val specs = specs0.filterNot(_.attr.contains(HasSourceCode))
      val res = specs.nonEmpty
      if (res) performSpecs(specs, f, revision = revision, thirdParty = thirdParty)
      res
    }
    if (hasFile) println(f.absolutePath)

  } catch {
    case NonFatal(e) =>
      Console.err.println(s"Error in '$name':")
      throw e
  }

  def performSpecs(specs: Seq[UGenSpec], file: File, revision: Int, thirdParty: Option[String]): Unit = {
    val out = new FileOutputStream(file)
    try {
      // create class trees
      val classes: List[Tree] = specs.flatMap(spec => performSpec(spec, thirdParty = thirdParty))(breakOut)

      val header =
        """package de.sciss.synth
          |package ugen
          |
          |import UGenSource._
          |""".stripMargin

      val strBody = classes.map(_.mkString).mkString(header, "\n", "")

      // convert the tree to plain text and write it to the output file
      // val strBody = createText(pkg)
      // we prepend a revision line comment which reflects the version
      // of the spec file used
      val strRev  = s"// revision: $revision\n$strBody"
      val bytes   = strRev.getBytes(CHARSET)
      out.write(bytes)
    } finally {
      out.close()
    }
  }

  // def compilationUnitOfFile(f: AbstractFile): Option[CompilationUnit] = unitOfFile.get(f)

  // Expects a paragraph already with newlines replaced by whitespace.
  private def linesWrap(para: String, width: Int): List[String] = {
    val sz = para.length
    if (sz <= width ) return para :: Nil

    val words0: List[String] = para.split("`").toList.zipWithIndex.flatMap { case (chunk, idx) =>
      if (idx % 2 == 0) chunk.split(" ").filterNot(_.isEmpty)
      else s"`$chunk`" :: Nil
    }

    val words = words0.map { w =>
      if (w.startsWith("[[")) {
        val i     = w.indexOf("]]")
        val link0 = w.substring(2, i)
        mkLink(link0) + w.substring(i + 2)
      } else {
        w
      }
    }

    val b     = List.newBuilder[String]
    val sb    = new StringBuilder

    def flush(): Unit =
      if (sb.nonEmpty) {
        b += sb.toString().trim
        sb.clear()
      }

    @tailrec def loop(rem: List[String]): List[String] = rem match {
      case head :: tail =>
        if (sb.nonEmpty && sb.length + head.length >= width) {
          flush()
          // in Scala-Doc Wiki Syntax, lines beginning with '='
          // are interpreted as headings! a simple work-around is
          // to prepend a non-breaking space, although visually
          // that may result in two space characters showing up.
          if (head.startsWith("=")) sb.append("&nbsp;")
        } else {
          sb.append(' ')
        }
        sb.append(head)
        loop(tail)

      case _ =>
        flush()
        b.result()
    }

    loop(words)
  }

  private def linesFromParagraphs(paras: List[String], width: Int = DocWidth): List[String] = {
    val b = List.newBuilder[String]
    @tailrec def loop(xs: List[String], feed: Boolean): Unit =
      xs match {
        case head :: tail =>
          val isPre = head.startsWith("{{{")
          val headL = if (isPre) {
            val i     = head.indexOf("}}}") + 3
            val headC = head.substring(0, i)
            val headN = head.substring(i)
            val code  = headC.split('\n').toList
            val norm  = linesWrap(headN, width)
            code ++ norm

          } else {
            if (feed) b += ""
            linesWrap(head, width)
          }
          b ++= headL
          loop(tail, feed = !isPre)

        case Nil =>
      }

    loop(paras, feed = false)
    b.result()
  }

  private def indent(in: String): String = {
    val lines = in.split('\n')
    lines.mkString("  ", "\n  ", "")
  }

  trait Tree {
    def mkString: String
  }

  private case class ParamDef(name: String, tpe: String, default: Tree = EmptyTree)
    extends Tree {

    def mkString: String = {
      val base = s"$name: $tpe"
      if (default == EmptyTree) base else s"$base = ${default.mkString}"
    }

    def hasDefault: Boolean = default != EmptyTree
  }

  private case object EmptyTree extends Tree {
    def mkString: String = ""
  }

  private case object This extends Tree {
    def mkString: String = "this"
  }

  private case class ValDef(name: String, tpe: Option[String] = None, rhs: Tree)
    extends Tree {

    def mkString: String = {
      val base  = tpe.fold(name)(t => s"name: $t")
      val res0  = if (rhs == EmptyTree) base else s"$base = ${rhs.mkString}"
      s"val $res0"
    }
  }

  private case class Apply(receiver: Tree, args: List[Tree]) extends Tree {
    def mkString: String = {
      val argsS     = args.map(_.mkString)
      val receiverS = receiver.mkString
      argsS.mkString(s"$receiverS(", ", ", ")")
    }
  }

  private case class Select(receiver: Tree, method: String) extends Tree {
    def mkString: String = {
      val receiverS = receiver.mkString
      s"$receiverS.$method"
    }
  }

  private case class Ident(name: String) extends Tree {
    def mkString: String = name
  }

  private case class Block(body: Tree*) extends Tree {
    def mkString: String = {
      val bodyS = body.map(_.mkString)
      bodyS.mkString(s"{\n  ", "\n  ", "\n}")
    }
  }

  private case class New(name: String, args: List[Tree]) extends Tree {
    def mkString: String = {
      val argsS = args.map(_.mkString)
      argsS.mkString(s"new $name(", ", ", ")")
    }
  }

  private case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree {
    def mkString: String = {
      val condS = cond.mkString
      val thenS = thenp.mkString
      val elseS = elsep.mkString
      val res0  = s"if ($condS) $thenS"
      if (elsep == EmptyTree) res0 else s"$res0 else $elseS"
    }
  }

  private case class BooleanLiteral(value: Boolean) extends Tree {
    def mkString: String = value.toString
  }

  private case class IntLiteral(value: Int) extends Tree {
    def mkString: String = value.toString
  }

  private case class FloatLiteral(value: Float) extends Tree {
    def mkString: String = s"${value}f"
  }

  private case class StringLiteral(value: String) extends Tree {
    def mkString: String = quote(value)
  }

  private case class MethodDef(name: String, tpe: List[String] = Nil, params: List[List[ParamDef]],
                               ret: String, body: Tree, isProtected: Boolean = false, isPrivate: Boolean = false,
                               scope: Option[String] = None, isOverride: Boolean = false) extends Tree {
    def mkString: String = {
      val tpeS    = if (tpe.isEmpty) "" else tpe.mkString("[", ", ", "]")
      val paramsS = params.map(sub => sub.map(_.mkString).mkString("(", ", ", ")")).mkString
      val abs     = s"def $name$tpeS$paramsS: $ret"
      val bodyS   = body.mkString
      val res0    = if (body == EmptyTree) abs else {
        if (!bodyS.startsWith("{") && abs.length + bodyS.length > LineWidth) {
          val bodyI = indent(bodyS)
          s"$abs = \n$bodyI"
        } else {
          s"$abs = $bodyS"
        }
      }
      val scopeS  = scope.fold("")(s => s"[$s]")
      val pre0    = if (isProtected) s"protected$scopeS " else if (isPrivate) s"private$scopeS " else ""
      val pre1    = if (isOverride) s"override $pre0" else pre0
      s"$pre1$res0"
    }
  }

  private case class CaseClassDef(name: String, tpe: List[String] = Nil, params: List[List[ParamDef]],
                                  parents: List[String], body: List[Tree],
                                  isProtected: Boolean = false, isPrivate: Boolean = false,
                                  scope: Option[String] = None,
                                  isFinal: Boolean = false) extends Tree {
    def mkString: String = {
      val tpeS    = if (tpe.isEmpty) "" else tpe.mkString("[", ", ", "]")
      val paramsS = params.map(sub => sub.map(_.mkString).mkString("(", ", ", ")")).mkString

      val parentsS = parents match {
        case Nil => ""
        case single :: rest =>
          val pre = s"extends $single"
          if (rest.isEmpty) pre else rest.mkString(s"$pre with ", " with ", "")
      }
      val abs0    = s"case class $name$tpeS$paramsS"
      val abs     = if (parentsS.isEmpty) {
        abs0
      } else if (abs0.length + parentsS.length > LineWidth) {
        s"case class $name$tpeS$paramsS\n  $parentsS"
      } else {
        s"case class $name$tpeS$paramsS $parentsS"
      }
      val res0    = if (body.isEmpty) abs else {
        val bodyS = indent(body.map(_.mkString).mkString("\n\n"))
        val space = if (abs.contains("\n")) "\n\n" else "\n"
        s"$abs {$space$bodyS\n}"
      }
      val scopeS  = scope.fold("")(s => s"[$s]")
      val pre0    = if (isProtected) s"protected$scopeS " else if (isPrivate) s"private$scopeS " else ""
      val pre1    = if (isFinal) s"${pre0}final " else pre0
      s"$pre1$res0"
    }
  }

  private case class ObjectDef(name: String, parents: List[String] = Nil, body: List[Tree])
    extends Tree {

    def mkString: String = {
      val parentsS = parents match {
        case Nil => ""
        case single :: rest =>
          val pre = s" extends $single"
          if (rest.isEmpty) pre else rest.mkString(s"$pre with ", " with ", "")
      }
      val abs = s"object $name$parentsS"
      val res0    = if (body.isEmpty) abs else {
        val bodyS = indent(body.map(_.mkString).mkString("\n\n"))
        s"$abs {\n$bodyS\n}"
      }
      res0
    }
  }

  private case class DocTree(comments: String, body: Tree) extends Tree {
    def mkString: String = s"$comments${body.mkString}"
  }

  private def wrapDoc(spec: UGenSpec, tree: Tree, body: Boolean, args: Boolean, examples: Boolean,
                      thirdParty: Option[String]): Tree = {
    if (spec.doc.isEmpty) return tree
    val doc       = spec.doc.get
    val bodyDoc   = if (body) doc.body  else Nil
    val linkDocs  = if (body) doc.links else Nil
    val argDocs   = if (args) collectMethodDocs(spec) else Nil
    val warnPos   = body && doc.warnPos

    val hasAny    = doc.body.nonEmpty || doc.links.nonEmpty || argDocs.nonEmpty || warnPos
    if (!hasAny) return tree

    def feed(pre: List[String], post: List[String]): List[String] =
      if (pre.isEmpty) post else pre ::: "" :: post

    val body0 = linesFromParagraphs(bodyDoc)

    val body1: List[String] = if (!examples || doc.examples.isEmpty) body0 else {
      // Note: the @example tag isn't really helpful. All it does is prepend the text with
      // a plain 'Examples:' line, plus inserting stupid commas.
      //
      //      val exList = doc.examples.flatMap { ex =>
      //        (s"@example ${ex.name}" :: "{{{" :: ex.code) :+ "}}}"
      //      }

      val exList: List[String] = doc.examples.flatMap { ex =>
        val codeLines: List[String] = ex.tpe match {
          case UGenSpec.Example.Simple =>
            ex.code match {
              case single :: Nil =>
                s"play { $single }" :: Nil
              case multi =>
                ("play {" :: multi.map(ln => s"  $ln")) :+ "}"
            }

          case UGenSpec.Example.Full => ex.code
        }
        // ScalaDoc fails to keep indentation when we start with `play {` straight away.
        // As a work-around move the example description into an initial comment.
        //
        // (ex.name :: "{{{" :: codeLines) :+ "}}}"
        ("{{{" :: s"// ${ex.name}" :: codeLines) :+ "}}}"
      }

      body0 ++ ("" :: "===Examples===" :: "" :: exList)
    }

    val body2 = thirdParty.fold(body1) { pluginName =>
      feed(body1, s"This is a third-party UGen ($pluginName)." :: Nil)
    }

    val body3 = if (!warnPos) body2 else {
      feed(body2, "@note The argument order is different from its sclang counterpart." :: Nil)
    }

    val body4 = if (argDocs.isEmpty) body3 else {
      val argLines = argDocs.flatMap { case (aName, aDoc) =>
        val aDocL = linesWrap(aDoc, DocWidth - ParamColumns) // 80 - 23 = 57 columns
        aDocL.zipWithIndex.map { case (ln, idx) =>
          val pre = if (idx == 0) ("@param " + aName).take(ParamColumns - 2) else ""
          val tab = pre + (" " * (ParamColumns - pre.length))
          tab + ln
        }
      }
      feed(body3, argLines)
    }

    val body5 = if (linkDocs.isEmpty) body4 else {
      val linkLines = linkDocs.map { link0 => s"@see ${mkLink(link0)}" }
      feed(body4, linkLines)
    }

    // `body == false` is used for methods; in that case do not prepend new-lines
    val comm = body5.mkString(if (body) s"\n/** " else s"/** ", "\n  * ", "\n  */\n")
    DocTree(comm, tree)
  }

  private def mkLink(link0: String): String = {
    val i         = link0.lastIndexOf('.')
    val link      = link0.substring(i + 1)
    val fullLink  = if (link0.startsWith("ugen."))
      s"de.sciss.synth.$link0$$"  // companion object. XXX TODO: not all UGens do have one... e.g. PV_BinWipe doesn't
    else if (link0.charAt(0).isUpper)
      s"de.sciss.synth.$link0"
    else
      link0
    s"[[$fullLink $link]]"
  }

  /*
   * Collects argument documentation in the form of a list of tuples argName -> joinedDoc. If there aren't
   * docs, returns `Nil`.
   */
  private def collectMethodDocs(spec: UGenSpec): List[(String, String)] = spec.doc match {
    case Some(doc) =>
      spec.args.flatMap { a =>
        val aDoc = doc.args.get(a.name)
        val init = a.tpe match {
          case ArgumentType.GE(_, true) => true
          case _ => false
        }
        val x   = if (init) aDoc.orElse(Some(Nil)) else aDoc
        val end = if (init) " ''(init-time only)''" else ""
        x.map { d =>
          a.name -> d.mkString("", " ", end)
        }
      } (breakOut)

    case _ => Nil
  }

  private implicit final class RichRate(val peer: Rate) {
    def traitTypeString = s"${peer.name.capitalize}Rated"
  }

  private def quote (s: String): String = "\"" + escape(s) + "\""
  private def escape(s: String): String = s.flatMap(escapedChar)

  private def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
    else              String.valueOf(ch)
  }

  private implicit final class RichArgumentValue(val peer: ArgumentValue) /* extends AnyVal */ {
    import ArgumentValue.{String => AString, _}

//    def mkString: String = peer match {
//      case Int(i)         => i.toString
//      case Float(f)       => s"${f}f"
//      case Boolean(b)     => (if (b) 1 else 0).toString   // currently no type class for GE | Switch
//      case AString(s)     => quote(s)                     // currently no type class for GE | String
//      case other          => other.toString
//    }

    def toTree: Tree = peer match {
      case Int(i)         => IntLiteral(i)
      case Float(f)       => FloatLiteral(f)
      case Boolean(b)     => IntLiteral(if (b) 1 else 0) // currently no type class for GE | Switch
      case AString(s)     => StringLiteral(s)               // currently no type class for GE | String
      case other          => Ident(other.toString)
//      case Inf            => Ident(strInf)
//      case DoneAction(a)  => Ident(a.name)
//      case Ny             => Ident(strNyquist)
    }
  }

  private implicit final class RichRateMap[A](val peer: Map[MaybeRate, A]) {
    def getWithDefault(r: MaybeRate): Option[A] = peer.get(r).orElse(peer.get(UndefinedRate))
  }

  private implicit final class RichArgument(val peer: UGenSpec.Argument) /* extends AnyVal */ {
    def typeString: String = peer.tpe match {
      case GE(Sig.String,_)   => "String" // currently no type class for GE | String
      case GE(Sig.DoneFlag,_) => "GE with HasDoneFlag"
      case GE(_,_)            => "GE"
      case ArgumentType.Int   => "Int"
    }

//    def defaultString(rate: MaybeRate = UndefinedRate): Option[String] =
//      peer.defaults.getWithDefault(rate).map(_.mkString)

    def defaultTree(rate: MaybeRate = UndefinedRate): Tree = peer.defaults.getWithDefault(rate) match {
      case Some(df) => df.toTree
      case _        => EmptyTree
    }

    def typeIsString: Boolean = peer.tpe match {
      case GE(Sig.String,_) => true
      case _                => false
    }

    def typeIsGE: Boolean = peer.tpe match {
      case GE(_,_) => true
      case _       => false
    }
  }

  private val strSideEffect   = "HasSideEffect"
  // private val traitSideEffect = TypeDef(Modifiers(Flags.TRAIT), typeName(strSideEffect), Nil, EmptyTree)
  private val strDoneFlag     = "HasDoneFlag"
  // private val traitDoneFlag   = TypeDef(Modifiers(Flags.TRAIT), typeName(strDoneFlag), Nil, EmptyTree)
  // val traitRandom       = TypeDef(Modifiers(Flags.TRAIT), "UsesRandSeed":  TypeName, Nil, EmptyTree)
  private val strIndiv        = "IsIndividual"
  // private val traitIndiv      = TypeDef(Modifiers(Flags.TRAIT), typeName(strIndiv), Nil, EmptyTree)
  // val traitWritesBuffer = TypeDef(Modifiers(Flags.TRAIT), "WritesBuffer":  TypeName, Nil, EmptyTree)
  // val traitWritesFFT    = TypeDef(Modifiers(Flags.TRAIT), "WritesFFT":     TypeName, Nil, EmptyTree)
  // val traitWritesBus    = TypeDef(Modifiers(Flags.TRAIT), "WritesBus":     TypeName, Nil, EmptyTree)

  private val strApply            = "apply"
  // private val identApply          = Ident(strApply)
  private val strVec              = "Vec"
  private val strVector           = "Vector"
  private val identVector         = Ident(strVector)
  private val identConstant       = Ident("Constant")
  private val strChannelProxy     = "ChannelProxy"
  private val identChannelProxy   = Ident(strChannelProxy)
  private val strMakeUGens        = "makeUGens"
  private val strMakeUGen         = "makeUGen"
  private val identMakeUGen       = Ident(strMakeUGen)
  private val strExpand           = "expand"
  private val strUArgs            = "_args"
  private val identUArgs          = Ident(strUArgs)
  private val identUnwrap         = Ident("unwrap")
  private val identThis           = Ident("this")
  private val strMaybeRate        = "MaybeRate"
  private val strRate             = "Rate"
  private val strMaybeResolve     = "getOrElse" // "?|"
  private val strOutputs          = "outputs"
  private val strUGenIn           = "UGenIn"
  private val strName             = "name"
  private val identName           = Ident(strName)
  private val strRateArg          = "rate"
  private val strRateMethod       = "rate"
  private val identRateArg        = Ident(strRateArg)
  private val strStringArg        = "stringArg"
  private val identStringArg      = Ident(strStringArg)
  private val identMatchRate      = Ident("matchRate")
  private val identMatchRateT     = Ident("matchRateT")
  private val identMatchRateFrom  = Ident("matchRateFrom")
  private val strEmpty            = "empty"
  private val strPlusPlus         = "++"
  private val strPlusColon        = "+:"
  private val strMinus            = "-"
  private val strSize             = "size"
  private val strFill             = "fill"
  private val identBinaryOp       = Ident("BinaryOpUGen")
  private val strMake1            = "make1"
  private val strTimes            = "Times"

  def performSpec(spec: UGenSpec, thirdParty: Option[String]): List[Tree] = {
    import spec.{name => uName, _}

    val impliedRate = rates match {
      case Rates.Implied(r, _) => Some(r)
      case _                   => None
    }

    // option for a binary operator expansion (synthetic mul-input)
    val expandBin = {
      val muls = args.filter(_.tpe match {
        case GE(Sig.Mul, _)  => true
        case _               => false
      })
      require(muls.size <= 1, s"Can only have one expandBin ($uName)")
      muls.headOption
    }

    // this is to ensure for makeUGen that when expandBin is defined, we simply do not pass in
    // _args to the UGen constructor, simplifying the process of filtering out the argument
    // which corresponds to expandBin!
    require(expandBin.isEmpty || inputs.size == 1, s"Currently `mul` input must be sole input ($uName)")

    val argsIn  = args.toList
    // val argsOut = args.filter(a => inputMap.contains(a.name))
    val argsOut = inputs.flatMap(i => argMap.get(i.arg)) // important: must be sorted according to `inputs`

    // whether the case class should contain default values or not
    val hasApply = rates.method match {
      case RateMethod.Custom(`strApply`) => true
      case RateMethod.Alias (`strApply`) => true
      case _                             => false
    }

    val sortedRates = rates.set.toList.sorted
    // the companion object's methods
    val objectMethodDefs: List[Tree] = sortedRates.flatMap { rate =>
      // e.g. `freq: GE, phase: GE = 0f`
      val objectMethodArgs: List[ParamDef] = argsIn.map { uArgInfo =>
        ParamDef(
          name    = uArgInfo.name,
          tpe     = uArgInfo.typeString,
          default = uArgInfo.defaultTree(rate)
        )
      }

      // either a parenthesised arg list, or no parens if there aren't any args
      val objectMethodArgsList: List[List[ParamDef]] =
        if (objectMethodArgs.nonEmpty) objectMethodArgs :: Nil else Nil

      // e.g. `apply(audio, freq, phase)
      // -- now replaced by `new SinOsc(audio, freq, phase)` to avoid overloading problems
      val methodBody: Tree = {
        val argIdents = argsIn.map(a => Ident(a.name))
        // prepend the rate argument if necessary
        val applyArgs = if (impliedRate.isEmpty)
          Ident(rate.name) :: argIdents
        else
          argIdents

        New(className, applyArgs)
      }

      val methodNames = rates.method match {
        case RateMethod.Default     => rate.methodName :: Nil
        case RateMethod.Custom(m)   => m :: Nil
        case RateMethod.Alias(m)    => rate.methodName :: m :: Nil
        // case RateMethod.None        => Nil
      }

      // e.g. `def ar(freq: GE, phase: GE = 0f): SinOsc`
      // e.g. `def apply(chain: GE, winType: GE = 0.0f, winSize: GE = 0.0f): IFFT`
      val fullMethods: List[(String, Tree)] = methodNames.map { mName =>
        val df = MethodDef(
          name    = mName,
          tpe     = Nil,
          params  = objectMethodArgsList,
          // Note: to help faster compilation of use site code, always produce the return type annotation
          // if (mName != strApply) EmptyTree else
          ret     = className, // TypeDef(NoMods, typeName(name), Nil, EmptyTree),
          body    = methodBody             // rhs
        )
        mName -> wrapDoc(spec, df, /* indent = 1, */ body = false, args = true, examples = false,
          thirdParty = None)
      }

      // whether the number of arguments is non-zero and there are defaults for all of them
      val allArgsHaveDefaults = objectMethodArgs.nonEmpty && objectMethodArgs.forall(_.hasDefault)

      val fullButApply = if (!hasApply) fullMethods else fullMethods.filterNot(_._1 == strApply)

      // ...in that case, we generate overloaded methods without parentheses
      if (allArgsHaveDefaults && methodNames.nonEmpty) {
        fullButApply.flatMap { case (mName, fullMethod) =>
          // e.g. `kr()`
          val overloadedBody = Apply(Ident(mName), Nil)
          // e.g. `def kr: SinOsc = kr()
          val overloadedMethod  = MethodDef(
            name    = mName,
            tpe     = Nil,    // tparams
            params  = Nil,    // vparams
            ret     = className,
            body    = overloadedBody  // rhs
          )
          overloadedMethod :: fullMethod :: Nil
        }
      } else {
        // Note: it's not possible to define `apply` on the companion object
        // for case classes, even if we omit the default arguments on the
        // case class constructor!
        // (if (forceCompanion) fullMethods else fullButApply).map(_._2)
        fullButApply.map(_._2)
      }
    }

    // the complete companion object. this is given as a List[Tree],
    // because there might be no object (in that case objectDef is Nil).
    val objectDef = if (forceCompanion || objectMethodDefs.nonEmpty) {
      val mod = ObjectDef(
        name    = className,
        parents = Nil,
        body    = objectMethodDefs  // body
      )
      wrapDoc(spec, mod, /* indent = 0, */ body = true, args = false, examples = true, thirdParty = thirdParty) :: Nil
    } else Nil

    // a `MaybeRate` is used when no rate is implied and an input generally uses a same-as-ugen constraint.
    // `maybeRateRef` contains the arguments which resolve the undefined rate at expansion time
    val maybeRateRef = if (impliedRate.isEmpty) {
      argsIn.filter(_.rates.get(UndefinedRate).contains(RateConstraint.SameAsUGen))
    } else Vector.empty

    val caseClassConstrArgs: List[ParamDef] = {
      // for each argument the tuple (NoMods, argName, type or type-and-default)
      val argTuples = argsIn map { arg =>
        val tpe = arg.typeString
        // Note: it's not possible to define `apply` on the companion object
        // for case classes, even if we omit the default arguments on the
        // case class constructor!
        ParamDef(name = arg.name, tpe = tpe, default = arg.defaultTree())
      }

      // if a rate is implied, we're done, otherwise prepend the rate argument
      if (impliedRate.isDefined) argTuples else {
        val tpe = if (maybeRateRef.nonEmpty) strMaybeRate else strRate
        ParamDef(strRateArg, tpe) :: argTuples
      }
    }

    // ---- attributes ----

    val a = spec.attr
    import Attribute._

    val readsBus      = a contains ReadsBus
    val readsBuf      = a contains ReadsBuffer
    val readsFFT      = a contains ReadsFFT
    val random        = a contains UsesRandSeed
    val indiv         = a contains IsIndividual

    val writesBus     = a contains WritesBus
    val writesBuf     = a contains WritesBuffer
    val writesFFT     = a contains WritesFFT
    val sideEffect    = a contains HasSideEffect

    val doneFlag      = a.contains(HasDoneFlag)

    val indSideEffect = writesBus || writesBuf || writesFFT
    val indIndiv      = readsBus  || readsBuf  || readsFFT || random || indSideEffect

    // generates the mixins such as `HasDoneFlag`, `IsIndividual`, and in the
    // case of an implied rate `AudioRated` etc.
    val caseClassMixins: List[String] = {

      val mixin1 = if (doneFlag)                    strDoneFlag   :: Nil    else Nil
      val mixin2 = if (indiv      || indIndiv)      strIndiv      :: mixin1 else mixin1
      val mixin3 = if (sideEffect || indSideEffect) strSideEffect :: mixin2 else mixin2

      impliedRate match {
        case Some(r)  => r.traitTypeString :: mixin3
        case _        => mixin3
      }
    }

    val expandResultStr = if (outputs.isEmpty) "Unit" else "UGenInLike"

    val hasVariadicOut  = outputs.exists(_.variadic.isDefined)
    val hasMultipleOuts = outputs.size > 1
    val multiOut        = hasMultipleOuts || hasVariadicOut
    val outputsPrefix = if (outputs.isEmpty)
      "Zero"
    else if (multiOut)
      "Multi"
    else
      "Single"

    // for each named output, we create a corresponding method in
    // the case class that produces a `ChannelProxy` instance.
    val namedOutputDefs: List[Tree] = if (!hasMultipleOuts) Nil else outputs.zipWithIndex.collect {
      case (UGenSpec.Output(Some(outName), _, _), idx) =>
        if (hasVariadicOut) {
          sys.error(s"The combination of variadic and namenamed outputs is not yet supported (${spec.name}, $outName)")
        }

        // ChannelProxy(this, idx)
        val methodBody = Apply(identChannelProxy, This :: IntLiteral(idx) :: Nil)

        MethodDef(
          name    = outName,
          tpe     = Nil,
          params  = Nil,
          ret     = "GE",
          body    = methodBody
        )
    } (breakOut)

    // `protected def makeUGens: UGenInLike = ...`
    val makeUGensDef: MethodDef = {
      val methodBody: Tree = {
        val argsApp: Vec[Tree] = argsOut.map { a =>
          val id = Ident(a.name)
          a.tpe match {
            case GE(Sig.String,_) => Apply(identStringArg, id :: Nil)
            case GE(_,_) =>
              val sel = Select(id, strExpand)
              inputMap(a.name).tpe match {
                case Input.Single => sel
                case Input.Variadic(prepSz) =>
                  val sel1 = Select(sel, strOutputs)
                  if (prepSz) {
                    Block(
                      ValDef(
                        name  = "_exp",
                        tpe   = None,
                        rhs   = sel1
                      ),
                      Apply(Select(Ident("_exp"), strPlusColon), 
                        Apply(identConstant, Select(Ident("_exp"), strSize) :: Nil) :: Nil)
                    )
                  } else sel1
              }

            case ArgumentType.Int => id
          }
        }

        def split(as: Vec[Argument], ts: Vec[Tree]): Vec[Tree] = {
          var from  = 0
          var keep  = false
          var b     = Vector.empty[Tree]
          val sz    = as.size
          while (from < sz) {
            val n = as.segmentLength(u => {
              val isMulti = inputMap(u.name).variadic || u.typeIsString
              isMulti == keep
            }, from)
            if (n > 0) {
              val slice = ts.slice(from, from + n)
              if (keep)
                b ++= slice
              else
                b :+= Apply(identVector, slice.toList)
              from += n
            }
            keep = !keep
          }
          b
        }

        val argsApp2 = split(argsOut, argsApp)
        argsApp2.lastOption match {
          case None =>
            Apply(identMakeUGen, Select(identVector, strEmpty) :: Nil) // shortcut, works because we do not use type apply
          case Some(l) =>
            Apply(identUnwrap, identThis :: argsApp2.init.foldRight(l)({
              (a, t) => Apply(Select(a, strPlusPlus), t :: Nil)
            }) :: Nil)
        }
      }

      MethodDef(
        isProtected = true,
        name    = strMakeUGens,
        tpe     = Nil,
        params  = Nil,
        ret     = expandResultStr,
        body    = methodBody
      )
    }

    // `protected def makeUGen(_args: Vec[UGenIn]): UGenInLike = ...`
    val makeUGenDef: MethodDef = {
      val methodBody: Tree = {
        val (preBody, ugenConstrArgs, rateConsArgs) = {
          // args1 are the last two args (isIndividual, hasSideEffect)
          val args1 = {
            // the last argument to UGen.SingleOut and UGen.MultiOut is `hasSideEffect`.
            // it has a default value of `false`, so we need to add this argument only
            // if there is a side effect and the UGen is not zero-out.
            // the preceding argument is `isIndividual` for all UGens

            val args0 = if (indiv || indIndiv) {
              Ident("isIndividual = true") :: Nil
            } else Nil

            if ((sideEffect || indSideEffect) && outputs.nonEmpty) {
              Ident("hasSideEffect = true") :: args0
            } else args0
          }

          // if the rate is of type `MaybeRate`, it will be resolved at the beginning of
          // the body and stored in local variable `_rate`. Otherwise we can use
          // `rate` straight away.
          val strResolvedRateArg  = if (maybeRateRef.nonEmpty) "_rate" else strRateArg
          val identResolvedRate   = Ident(impliedRate.map(_.name).getOrElse(strResolvedRateArg))

          // for a UGen.MultiOut the next preceding arg is `outputRates: Vec[Rate]`
          val args2 = if (!multiOut) Nil else {
            val numFixed  = outputs.count(_.variadic.isEmpty)
            val variadic  = outputs.collect {
              case Output(_ /* oName */, _, Some(v)) => v
            }
            if (numFixed > 0 && variadic.nonEmpty) {
              sys.error(s"Mixed variadic / non-variadic outputs not supported, in $uName")
            }
            if (variadic.size > 1) {
              sys.error(s"Multiple variadic outputs not supported, in $uName")
            }

            // tree that defines the number of outputs
            val numOutsTree = variadic.headOption match {
              case Some(v) =>
                val refArg = argMap(v)
                refArg.tpe match {
                  case GE(Sig.String,_) => sys.error(s"Strings not supported for variadic outputs, in $uName")
                  case GE(_,_) =>
                    val numFixedArgs = argsOut.size - 1       // we ensured above that there is only one variadic argument
                    val selSz = Select(identUArgs, strSize)   // `_args.size`
                    if (numFixedArgs == 0) selSz else {
                      // `_args.size.-(<numFixedArgs>)`
                      Apply(Select(selSz, strMinus), IntLiteral(numFixedArgs) :: Nil)
                    }
                  case ArgumentType.Int =>
                    Ident(refArg.name)
                }

              case _ => IntLiteral(numFixed)
            }

            // `Vector.fill(<numOuts>)(<resolvedRate>)
            Apply(Apply(Select(identVector, strFill), numOutsTree :: Nil), identResolvedRate :: Nil) :: Nil
          }

          // rate constraints resolution
          // XXX TODO: if multiple arguments have the same constraints,
          // we could optimize and have one common `if ... else` expression.
          // (e.g. see `DecodeB2`)
          val (rateConsTrees, rateConsArgs0, _) = ((List.empty[Tree], identUArgs, 0) /: argsOut.zipWithIndex) {
            case (res @ (_consTrees, _consArgs, _consCount), (arg, argIdx)) =>
              // the rate to which the ugen input is forced
              def implCons(cons: RateConstraint, sameRate: Ident = identResolvedRate)
                          (rhsFun: Tree => Tree = identity): (List[Tree], Ident, Int) = {
                val tgt = cons match {
                  case RateConstraint.SameAsUGen  => sameRate
                  case RateConstraint.Fixed(r)    => Ident(r.name)
                }
                val newCount  = _consCount + 1
                val newArgs   = s"${identUArgs.name}$newCount"
                val isTrig    = arg.tpe match {
                  case ArgumentType.GE(Sig.Trigger, _) => true
                  case _ => false
                }
                val isMulti   = inputMap(arg.name).variadic
                if (isTrig && isMulti)
                  sys.error(s"The combination of variadic trigger input and rate constraints is not yet supported (${spec.name})")

                // val <newArgs> = matchRate(<oldArgs>, argIdx, tgt)
                val matchMethod = if (isTrig) identMatchRateT else if (isMulti) identMatchRateFrom else identMatchRate
                val df = ValDef(
                  name  = newArgs,
                  tpe   = None,
                  rhs   = rhsFun(Apply(matchMethod, _consArgs :: IntLiteral(argIdx) :: tgt :: Nil))
                )
                val newTrees = _consTrees ++ List(df)
                (newTrees, Ident(newArgs), newCount)
              }

              arg.rates.get(UndefinedRate).fold {
                (res /: List(audio, control)) { (res1, rateKey) =>
                  arg.rates.get(rateKey).fold(res1) { cons =>
                    // ---- constraint only if host runs at audio-rate ----
                    val identRateKey = Ident(rateKey.name)
                    implCons(cons, identRateKey) { matchRateCall =>
                      // if (identResolvedRate == tgt) matchRate(...) else oldArgs
                      val cond = Apply(Select(identResolvedRate, "=="), identRateKey :: Nil)
                      If(cond, matchRateCall, _consArgs)
                    }
                  }
                }
              } { cons =>
                // ---- constraint for any host rate ----
                implCons(cons)()
              }
          }

          // might support Mul besides other args in the future
          val inArg = if (expandBin.isDefined) {
            require(argsOut.size == 1, s"At present, Mul input must be only input, in $uName")
            Select(identVector, strEmpty) // `inputs = Vector.empty`
          } else {
            rateConsArgs0  // `inputs = _args`
          }

          // args3 = (outputRates, inputs, isIndividual, [hasSideEffect])
          val args3 = args2 ::: inArg :: args1
          // args = (name, rate, outputRates, inputs, isIndividual, [hasSideEffect]) = complete list
          val treeUGenName = if (uName == className) identName else StringLiteral(uName)
          val args4 = treeUGenName :: identResolvedRate :: args3

          // when using a MaybeRate, the preBody is the code that resolves that rate
          val _preBody = if (maybeRateRef.isEmpty) rateConsTrees else {
            // XXX TODO: deal with multiple refs!
            maybeRateRef.headOption.fold(rateConsTrees) { ua =>
              val aPos = argsOut.indexOf(ua)
              require(argsOut.take(aPos).forall(a => a.typeIsGE && !inputMap(a.name).variadic), "Cannot resolve MaybeRate ref after multi args")
              // `val _rate = rate |? _args(<pos>).rate`
              val rateResolution = ValDef(
                name = strResolvedRateArg,
                tpe = None,
                rhs = Apply(Select(identRateArg, strMaybeResolve),
                  Select(Apply(identUArgs, IntLiteral(aPos) :: Nil), strRateMethod) :: Nil)
              )
              rateResolution :: rateConsTrees
            }
          }

          (_preBody, args4 :: Nil, rateConsArgs0) // only single argument list
        }

        val ugenTpe     = s"UGen.${outputsPrefix}Out"
        // val ugenConstr = New(ugenTpe, ugenConstrArgs)
        val ugenConstr  = Apply(Ident(ugenTpe), ugenConstrArgs.head)

        // the resulting application is either the ugen instantiation, or, if a Mul is used,
        // a binary op of the ugen instantiation
        val resApp = expandBin match {
          case Some(mul) =>
            val aPos      = argsOut.indexOf(mul)
            val mulMake1  = Select(Select(identBinaryOp, strTimes), strMake1)
            Apply(mulMake1, ugenConstr :: Apply(rateConsArgs, IntLiteral(aPos) :: Nil) :: Nil)

          case _ => ugenConstr
        }

        // the complete body is the concatenation of the preBody (if it exists) and the ugen instantiation
        preBody match {
          case Nil    => resApp
          case trees  => Block(trees ++ List(resApp): _*)
        }
      }

      val methodArgs = List(List(ParamDef(
        name  = strUArgs,
        tpe   = s"$strVec[$strUGenIn]"
      )))

      MethodDef(
        isProtected = true,
        name        = strMakeUGen,
        tpe         = Nil,
        params      = methodArgs,
        ret         = expandResultStr,
        body        = methodBody
      )
    }

    val caseClassMethods = {
      val m1 = makeUGensDef :: makeUGenDef :: namedOutputDefs
      m1
    }

//    val caseClassMethods = if (className == uName) caseClassMethods0 else {
//      val mName = MethodDef(
//        isOverride  = true,
//        name        = strName,
//        tpe         = Nil,
//        params      = Nil,
//        ret         = strString,
//        body        = methodBody
//      )
//      mName :: caseClassMethods0
//    }

    // e.g. `UGenSource.ZeroOut` or `UGenSource.SingleOut`, ...
    val outputsTypeString = s"UGenSource.${outputsPrefix}Out"
    // super class and traits
    val caseClassParents  = outputsTypeString :: caseClassMixins

    val caseClassDef =
      CaseClassDef(
        isFinal = true,
        name    = className,
        tpe     = Nil, // tparams
        params  = caseClassConstrArgs :: Nil,
        body    = caseClassMethods,
        parents = caseClassParents  // parents
      )

    val caseClassWithDoc = wrapDoc(spec, caseClassDef, body = true, args = true, examples = false,
      thirdParty = thirdParty)
    objectDef ::: (caseClassWithDoc :: Nil)
  }
}