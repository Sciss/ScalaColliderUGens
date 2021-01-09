package de.sciss.synth

import UGenSource._
import ugen._

object ThirdPartyUGens extends Map[String, ProductReader[Product]] {
  type V = ProductReader[Product]

  private val peer = Map[String, V](
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

  def removed(key: String): Map[String, V] = peer.removed(key)

  def updated[V1 >: V](key: String, value: V1): Map[String, V1] = peer.updated(key, value)

  def get(key: String): Option[V] = peer.get(key)

  def iterator: Iterator[(String, V)] = peer.iterator
}
