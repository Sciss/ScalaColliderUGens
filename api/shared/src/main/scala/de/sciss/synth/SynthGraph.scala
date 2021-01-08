/*
 *  SynthGraph.scala
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

import de.sciss.synth.impl.SynthGraphBuilderImpl
import de.sciss.synth.ugen.ControlProxyLike

import scala.collection.immutable.{IndexedSeq => Vec}

object SynthGraph {
  trait Builder {
    def addLazy(g: Lazy): Unit

    def addControlProxy(proxy: ControlProxyLike): Unit
  }

  // java.lang.ThreadLocal is around 30% faster than
  // using a synchronized map, plus we don't need
  // to look after its cleaning
  private val builders = new ThreadLocal[Builder] {
    override protected def initialValue: Builder = BuilderDummy
  }

  def builder: Builder = builders.get

  def apply(thunk: => Any): SynthGraph = {
    val b = new SynthGraphBuilderImpl
    use(b) {
      thunk
      b.build
    }
  }

  /** Installs a custom synth graph builder on the current thread,
    * during the invocation of a closure. This method is typically
    * called from other libraries which wish to provide a graph
    * builder other than the default.
    *
    * When the method returns, the previous graph builder has automatically
    * been restored. During the execution of the `body`, calling
    * `SynthGraph.builder` will return the given `builder` argument.
    *
    * @param builder    the builder to install on the current thread
    * @param body       the body which is executed with the builder found through `SynthGraph.builder`
    * @tparam A         the result type of the body
    * @return           the result of executing the body
    */
  def use[A](builder: Builder)(body: => A): A = {
    val old = builders.get()
    builders.set(builder)
    try {
      body
    } finally {
      builders.set(old)
    }
  }

  /** A boolean setting (defaults to `false`) which can help track down
    * bugs with graph elements being added outside a `SynthGraph` context.
    * When this setting is `true`, a warning message is printed to
    * `Console.err` with the graph element added and the stack trace,
    * indicating calls such as `SinOsc.ar` outside a
    * thread local `SynthGraph` builder.
    */
  var warnOutsideContext = false

  private object BuilderDummy extends Builder {
    private def warn(obj: => String): Unit =
      if (warnOutsideContext) {
        Console.err.println("Warning - adding SynthGraph element outside context: " + obj)
        val e   = new Throwable()
        e.fillInStackTrace()
        val t   = e.getStackTrace
        val n   = t.length
        var i   = 0
        var go  = true
        while (i < n && go) {
          val c = t(i).getClassName
          if ((c.startsWith("de.sciss.synth.") &&
            (c.charAt(15).isUpper || c.startsWith("de.sciss.synth.ugen."))) ||
            c.startsWith("scala.collection.")) {
            i += 1
          } else {
            go = false
          }
        }
        while (i < n) {
          Console.err.println("  at " + t(i))
          i += 1
        }
      }

    def addLazy(g: Lazy): Unit = warn(g.toString)

    def addControlProxy(proxy: ControlProxyLike): Unit = warn(proxy.toString)
  }
}

final case class SynthGraph(sources: Vec[Lazy], controlProxies: Set[ControlProxyLike]) {
  def isEmpty : Boolean  = sources.isEmpty && controlProxies.isEmpty
  def nonEmpty: Boolean  = !isEmpty
  def expand(implicit factory: UGenGraph.BuilderFactory): UGenGraph = factory.build(this)
}