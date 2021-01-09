package de.sciss.synth

import UGenSource._
import ugen._

object ThirdPartyUGens extends Map[String, ProductReader[Product]] {
  type V = ProductReader[Product]

  private val peer = Map[String, V](
    ("JPverbRaw", JPverbRaw),
    ("GreyholeRaw", GreyholeRaw),
    ("ComplexRes", ComplexRes),
    ("DiodeRingMod", DiodeRingMod),
    ("DNoiseRing", DNoiseRing),
    ("RMS", RMS),
    ("Logger", Logger),
    ("ListTrig", ListTrig),
    ("ListTrig2", ListTrig2),
    ("BufMax", BufMax),
    ("BufMin", BufMin),
    ("ArrayMax", ArrayMax),
    ("ArrayMin", ArrayMin),
    ("InsideOut", InsideOut),
    ("WaveLoss", WaveLoss),
    ("Squiz", Squiz),
    ("MdaPiano", MdaPiano),
    ("DFM1", DFM1),
    ("VBAP", VBAP),
    ("CircleRamp", CircleRamp),
    ("Hopf", Hopf),
  )

  def removed(key: String): Map[String, V] = peer.removed(key)

  def updated[V1 >: V](key: String, value: V1): Map[String, V1] = peer.updated(key, value)

  def get(key: String): Option[V] = peer.get(key)

  def iterator: Iterator[(String, V)] = peer.iterator
}
