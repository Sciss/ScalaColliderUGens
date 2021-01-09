/*
 *  Control.scala
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

import de.sciss.synth.UGenSource._

import scala.collection.immutable.{IndexedSeq => Vec}

// ---------- Control ----------

object Control extends ProductReader[Control] {
  /** Note: we are not providing further convenience methods,
    * as that is the task of ControlProxyFactory...
    */
  def ir(values: ControlValues, name: Option[String] = None): Control = apply(scalar , values.seq, name)
  def kr(values: ControlValues, name: Option[String] = None): Control = apply(control, values.seq, name)

  def read(in: RefMapIn, arity: Int): Control = {
    require (arity == 3)
    val _rate     = in.readRate()
    val _values   = in.readFloatVec()
    val _ctrlName = in.readStringOption()
    new Control(_rate, _values, _ctrlName)
  }
}

final case class Control(rate: Rate, values: Vec[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut {

  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    impl.ControlImpl(name, rate, numChannels = values.size, specialIndex = specialIndex)
  }
}

object ControlProxy extends ProductReader[ControlProxy] {
  private val scalarFactory  = new ControlFactory(scalar )
  private val controlFactory = new ControlFactory(control)

  def read(in: RefMapIn, arity: Int): ControlProxy = {
    require (arity == 3)
    val _rate     = in.readRate()
    val _values   = in.readFloatVec()
    val _name     = in.readStringOption()
    new ControlProxy(_rate, _values, _name)
  }
}
final case class ControlProxy(rate: Rate, values: Vec[Float], name: Option[String])
  extends ControlProxyLike {

  private[synth] def factory: ControlFactoryLike = rate match {
    case `scalar`   => ControlProxy.scalarFactory
    case `control`  => ControlProxy.controlFactory
    case _          => sys.error(s"Unsupported rate $rate")
  }
}

final class ControlFactory(rate: Rate) extends ControlFactoryLike {
  protected def makeUGen(numChannels: Int, specialIndex: Int): UGen =
    impl.ControlImpl("Control", rate, numChannels = numChannels, specialIndex = specialIndex)
}

// ---------- TrigControl ----------

object TrigControl extends ProductReader[TrigControl] {
  def kr(values: ControlValues, name: Option[String] = None): TrigControl = apply(values.seq, name)

  def read(in: RefMapIn, arity: Int): TrigControl = {
    require (arity == 2)
    val _values   = in.readFloatVec()
    val _ctrlName = in.readStringOption()
    new TrigControl(_values, _ctrlName)
  }
}

final case class TrigControl(values: Vec[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut with ControlRated /* with IsControl */ {

  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    impl.ControlImpl(name, control, numChannels = values.size, specialIndex = specialIndex)
  }
}

object TrigControlProxy extends ProductReader[TrigControlProxy] {
  private object factory extends ControlFactoryLike {
    protected def makeUGen(numChannels: Int, specialIndex: Int): UGen =
      impl.ControlImpl("TrigControl", control, numChannels = numChannels, specialIndex = specialIndex)
  }

  def read(in: RefMapIn, arity: Int): TrigControlProxy = {
    require (arity == 2)
    val _values   = in.readFloatVec()
    val _name     = in.readStringOption()
    new TrigControlProxy(_values, _name)
  }
}
final case class TrigControlProxy(values: Vec[Float], name: Option[String])
  extends ControlProxyLike with ControlRated {
  private[synth] def factory: ControlFactoryLike = TrigControlProxy.factory
}

// ---------- AudioControl ----------

object AudioControl extends ProductReader[AudioControl] {
  def ar(values: ControlValues, name: Option[String] = None): AudioControl = apply(values.seq, name)

  def read(in: RefMapIn, arity: Int): AudioControl = {
    require (arity == 2)
    val _values   = in.readFloatVec()
    val _ctrlName = in.readStringOption()
    new AudioControl(_values, _ctrlName)
  }
}

final case class AudioControl(values: Vec[Float], ctrlName: Option[String])
  extends UGenSource.MultiOut with AudioRated /* with IsControl */ {

  protected def makeUGens: UGenInLike = makeUGen(Vector.empty)

  protected def makeUGen(args: Vec[UGenIn]): UGenInLike = {
    val specialIndex = UGenGraph.builder.addControl(values, ctrlName)
    impl.ControlImpl(name, audio, values.size, specialIndex)
  }
}

object AudioControlProxy extends ProductReader[AudioControlProxy] {
  private object factory extends ControlFactoryLike {
    protected def makeUGen(numChannels: Int, specialIndex: Int): UGen =
      impl.ControlImpl("AudioControl", audio, numChannels = numChannels, specialIndex = specialIndex)
  }

  def read(in: RefMapIn, arity: Int): AudioControlProxy = {
    require (arity == 2)
    val _values   = in.readFloatVec()
    val _name     = in.readStringOption()
    new AudioControlProxy(_values, _name)
  }
}
final case class AudioControlProxy(values: Vec[Float], name: Option[String])
  extends ControlProxyLike with AudioRated {
  private[synth] def factory: ControlFactoryLike = AudioControlProxy.factory
}
