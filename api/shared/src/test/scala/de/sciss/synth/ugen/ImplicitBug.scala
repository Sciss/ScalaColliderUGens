package de.sciss.synth
package ugen

import language.implicitConversions

object ImplicitBug {
  implicit def namedControl(n: String): ControlProxyFactory = new ControlProxyFactory(n)
  val a1 = "amp".kr(0)
  val a2 = "amp".kr(Seq(0, 1, 2))
  val a3 = "amp".kr
  // val a4 = "amp".kr(1, 2, 3)
}