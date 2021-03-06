package de.sciss.synth

import UGenSource._
import ugen._

object ThirdPartyUGens {
  private lazy val _init: Unit = UGenSource.addProductReaders(map)

  def init(): Unit = _init

  type V = ProductReader[Product]

  private def map = Map[String, V](
    ("ArrayMax", ArrayMax),
    ("ArrayMin", ArrayMin),
    ("BufMax", BufMax),
    ("BufMin", BufMin),
    ("CircleRamp", CircleRamp),
    ("ComplexRes", ComplexRes),
    ("DFM1", DFM1),
    ("DNoiseRing", DNoiseRing),
    ("DiodeRingMod", DiodeRingMod),
    ("Greyhole", Greyhole),
    ("Hopf", Hopf),
    ("InsideOut", InsideOut),
    ("JPverb", JPverb),
    ("ListTrig", ListTrig),
    ("ListTrig2", ListTrig2),
    ("Logger", Logger),
    ("MdaPiano", MdaPiano),
    ("RMS", RMS),
    ("Squiz", Squiz),
    ("VBAP", VBAP),
    ("WaveLoss", WaveLoss),
  )
}
