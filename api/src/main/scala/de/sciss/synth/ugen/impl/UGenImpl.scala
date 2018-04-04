/*
 *  UGenImpl.scala
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
package impl

import scala.collection.immutable.{IndexedSeq => Vec}

final class ZeroOutImpl(val name: String, val rate: Rate, val inputs: Vec[UGenIn], val isIndividual: Boolean,
                        val specialIndex: Int)
  extends UGen.ZeroOut

final class SingleOutImpl(val name: String, val rate: Rate, val inputs: Vec[UGenIn], val isIndividual: Boolean,
                          val hasSideEffect: Boolean, val specialIndex: Int)
  extends UGen.SingleOut

final class MultiOutImpl(val name: String, val rate: Rate, val outputRates: Vec[Rate], val inputs: Vec[UGenIn],
                         val isIndividual: Boolean, val hasSideEffect: Boolean, val specialIndex: Int)
  extends UGen.MultiOut

// side-effect: receiving messages from clients!
// and more importantly: control ugens created from proxies are not wired, so they would
// be eliminated if side-effect was false!!!
object ControlImpl {
  def apply(name: String, rate: Rate, numChannels: Int, specialIndex: Int): ControlImpl = {
    val res = new ControlImpl(name, rate, numChannels = numChannels, specialIndex = specialIndex)
    UGenGraph.builder.addUGen(res)
    res
  }
}
final class ControlImpl private(val name: String, val rate: Rate, numChannels: Int, val specialIndex: Int)
  extends UGen.MultiOut {

  def outputRates: Vec[Rate] = Vector.fill(numChannels)(rate)

  def isIndividual : Boolean = false
  def hasSideEffect: Boolean = true

  def inputs: Vec[UGenIn] = Vector.empty
}

final class RawUGenImpl(val name: String, val rate: Rate, val numInputs: Int, val outputRates: Vec[Rate],
                        val specialIndex: Int) extends RawUGen {
  def numOutputs: Int = outputRates.size

  override def toString: String = {
//    val inputsS = inputs.map {
//      case Constant(f)                      => f.toString
//      case UGenOutProxy       (source, idx) => s"${source.name}[$idx]"
//      case ControlUGenOutProxy(source, idx) => s"${source.name.getOrElse("<control>")}[$idx]"
//      case ugen: UGen                       => ugen.name
//    }
    val no = numOutputs
    val numOutputsS = if (no           == 1) "" else s", numOutputs = $no"
    val specialS    = if (specialIndex == 0) "" else s", specialIndex = $specialIndex"
    // ${inputsS.mkString("[", ", ", "]")}
    s"UGen($name, $rate, numInputs = $numInputs$numOutputsS$specialS)"
  }
}
