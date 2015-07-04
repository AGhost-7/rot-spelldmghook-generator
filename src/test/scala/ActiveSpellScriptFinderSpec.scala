

import org.scalatest._

class ActiveSpellScriptFinderSpec extends FlatSpec with ShouldMatchers {
	val finder = new ActiveSpellScriptFinder {}

	// these are coupled with the 2da row.
	val spells = finder.findAllActiveSpellScripts
	// this is just the script name list.
	val scripts = spells.map { _._1 }
	// labels column in the 2da file
	val labels = spells.map { _._2("Label") }

	it should "include stock spells" in {
		scripts should contain("NW_S0_IceStorm".toLowerCase)
		scripts should contain("NW_S0_CloudKill".toLowerCase)
		labels should contain("Ice_Storm")
		labels should contain("Cloudkill")
	}

	it should "not include legacy nwn1 spells" in {
		scripts should not contain("NW_S0_TimeStop".toLowerCase)
	}


}
