package de.sciss.synth.ugen

import org.scalatest.FlatSpec
import de.sciss.synth.UGenSpec

/*
 To run only this test:

 test-only de.sciss.synth.ugen.ParserSpec

 */
class ParserSpec extends FlatSpec {
  "UGenSpec" should "be able to parse a specific entry" in {
    val is = getClass.getResourceAsStream("ChaosUGens.xml")
    try {
      val doc   = xml.XML.load(is)
      val cusp  = (doc \\ "ugen").find(_.attributes.asAttrMap("name") == "CuspN").get
      UGenSpec.parse(cusp, docs = true)
    } finally {
      is.close()
    }
  }

  "UGenSpec" should "be able to read in the standard definitions" in {
    val specs = UGenSpec.standardUGens
    assert(specs.size >= 360, s"Only ${specs.size} specs decoded (should be >=360)")
  }

  "A ClassGenerator" should "be able to process a spec" in {
    val cusp  = UGenSpec.standardUGens("Dseries")
    val gen   = new ClassGenerator
    gen.performSpec(cusp, thirdParty = None)
  }
}