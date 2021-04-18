package de.sciss.synth

import de.sciss.serial.{DataInput, DataOutput}
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

/*
 To run only this test:

 testOnly de.sciss.synth.SerializationSpec

 */
class SerializationSpec extends AnyFlatSpec {
  val VERBOSE = false

  "Standard and PlugIn UGens" should "be able to initialize" in {
    StandardUGens   .init()
    ThirdPartyUGens .init()
  }

  "Various UGens" should "be able to serialize and deserialize" in {
    implicit def stringToControl(name: String): ugen.ControlProxyFactory =
      new ugen.ControlProxyFactory(name)

    val g1 = SynthGraph {
      import ugen._
      import GEOps.fromGE

      val f1  = "freq1"   kr 0.4
      val f2  = "freq2"   kr 8.0
      val d   = "detune"  kr 0.90375
      val f   = LFSaw.ar(f1).mulAdd(24, LFSaw.ar(Seq(f2, f2 * d)).mulAdd(3, 80)).midiCps // glissando function
      val res = CombN.ar(SinOsc.ar(f) * 0.04, 0.2, 0.2, 4) // echoing sine wave
      Out.ar(0, res)
    }

    val g2 = SynthGraph {
      import ugen._
      import GEOps.fromGE

      val gen = EnvGen.kr(Env.adsr(), "gate".kr(0), doneAction = "done".kr(0))
      val osc = SinOsc.ar(440) * gen * 0.1
      WrapOut(osc)
    }

    StandardUGens   .init()
    ThirdPartyUGens .init()

    Seq(g1, g2).foreach { gIn =>
      val out   = DataOutput()
      val rOut  = new UGenSource.RefMapOut(out)
      rOut.writeElem(gIn)
      val arr   = out.toByteArray

      if (VERBOSE) {

        val HEX = "0123456789ABCDEF".getBytes

        import java.io.PrintStream
        import java.nio.{Buffer, ByteBuffer}

        def printHexOn(b: ByteBuffer, stream: PrintStream): Unit = {
          val pos0: Int = b.position
          try {
            val lim: Int = b.limit
            val txt = new Array[Byte](74)

            var j = 0
            var k = 0
            var m = 0
            var n = 0
            var i = 4
            while (i < 56) {
              txt(i) = 0x20.toByte
              i += 1
            }
            txt(56) = 0x7C.toByte

            stream.println()
            (b: Buffer).position(0)
            i = 0; while (i < lim) {
              j = 0
              txt(j) = HEX((i >> 12) & 0xF); j += 1
              txt(j) = HEX((i >>  8) & 0xF); j += 1
              txt(j) = HEX((i >>  4) & 0xF); j += 1
              txt(j) = HEX( i        & 0xF); j += 1
              m = 57
              k = 0
              while ((k < 16) && (i < lim)) {
                j += (if ((k & 7) == 0) 2 else 1)
                n = b.get()
                txt(j) = HEX((n >> 4) & 0xF); j += 1
                txt(j) = HEX( n       & 0xF); j += 1
                txt(m) = if ((n > 0x1F) && (n < 0x7F)) n.toByte else 0x2E.toByte; m += 1
                k += 1
                i += 1
              }
              txt(m) = 0x7C.toByte; m += 1
              while (j < 54) {
                txt(j) = 0x20.toByte; j += 1
              }
              while (m < 74) {
                txt(m) = 0x20.toByte; m += 1
              }
              stream.write(txt, 0, 74)
              stream.println()
            }
            stream.println()
          } finally {
            (b: Buffer).position(pos0)
          }
        }

        printHexOn(ByteBuffer.wrap(arr), Console.out)
      }

      val in    = DataInput(arr)
      val rIn   = new UGenSource.RefMapIn(in)
      val gOut = rIn.readGraph()

      assert (gOut == gIn)
    }
  }
}