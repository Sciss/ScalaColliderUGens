package de.sciss.synth.ugen

import de.sciss.synth.UGenSpec
import de.sciss.synth.UGenSpec.Parsed
import org.scalatest.flatspec.AnyFlatSpec

/*
 To run only this test:

 testOnly de.sciss.synth.ugen.ParserSpec

 */
class ParserSpec extends AnyFlatSpec {
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
    val cuspP = new Parsed {
      val typeId: Int       = -1
      val spec  : UGenSpec  = cusp
    }
    val gen   = new ClassGenerator
    gen.performSpec(cuspP, thirdParty = None)
  }
}