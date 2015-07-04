
import org.scalatest._
import org.scalatest.enablers.Emptiness

import scala.util.{Try, Success, Failure}

class SpellExtractorSpec extends FunSpec with ShouldMatchers {

	import SpellExtractor._

	implicit val tryEmptiness = new Emptiness[Try[_]] {
		def isEmpty(t: Try[_]): Boolean = t.isFailure
	}

	val scriptsT = activeNssSpellFiles

	val (hspells, errs) = getTargetScripts(scriptsT)

	println("total number of active scripts picked: " +
		scriptsT.filter { _.isSuccess }.length)
	println("total number of hooked spells: " + hspells.length)
	println("total number of errors: " + errs.length)

	def findRow(label: String) = scriptsT.find {
			case Success(mem) => mem.row("Label") == label
			case _ => false
		}.get.get.row

	describe("icestorm parsing results") {
		val file = FileLocator.nss("NW_S0_IceStorm")
		val lines = FileLocator.nssLines("NW_S0_IceStorm").toList
		val row = findRow("Ice_Storm")

		it("should have blud and cold damage types") {
			val typesT = extractDamageTypes(file, lines)
			typesT.isSuccess should be (true)
			val types = typesT.get
			types should contain("DAMAGE_TYPE_BLUDGEONING")
			types should contain("DAMAGE_TYPE_COLD")
		}

		it("should be flagged as being an AoE") {
			extractIsAoE(lines) should be (true)
		}

		it("should have the evocation school") {
			val schoolT = find2daSchool(row)
			schoolT should not be empty
			schoolT.get should be ("SPELL_SCHOOL_EVOCATION")
		}

		it("should have a spell level of 4") {
			val spellLvlT = extractSpellLevel(row)
			spellLvlT.get should be (4)
		}
	}

	describe("flame arrow parsing results") {
		val file = FileLocator.nss("NW_S0_FlmArrow")
		val lines = FileLocator.nssLines("NW_S0_FlmArrow").toList
		val row = findRow("Flame_Arrow")

		it("should have only fire damage") {
			val typesT = extractDamageTypes(file, lines)
			typesT should not be empty
			val types = typesT.get
			types should be === List("DAMAGE_TYPE_FIRE")
		}

		it("should be flagged as not being an AoE") {
			extractIsAoE(lines) should be (false)
		}

		it("should have the evocation school") {
			val schoolT = find2daSchool(row)
			schoolT should not be empty
			schoolT.get should be ("SPELL_SCHOOL_CONJURATION")
		}

		it("should have a spell level 3") {
			val spellLvlT = extractSpellLevel(row)
			spellLvlT.get should be (3)
		}
//		it("should be multihit") {
//			extractIsMultiHit(lines) should be (true)
//		}

	}

	describe("Orb of Energy parsing results") {
		val file = FileLocator.nss("cmi_s0_orbforce")
		val lines = FileLocator.nssLines("cmi_s0_orbforce").toList
		val row = findRow("Orb_Force")

		it("should have only energy damage") {
			val typesT = extractDamageTypes(file, lines)
			typesT should not be empty
			val types = typesT.get
			types should be === List("DAMAGE_TYPE_MAGICAL")
		}

		it("should have been flagged as a non-AoE") {
			extractIsAoE(lines) should be (false)
		}

		it("should have the conjuration school") {
			val schoolT = find2daSchool(row)
			schoolT should not be empty
			schoolT.get should be ("SPELL_SCHOOL_CONJURATION")
		}

		it("should have a spell level of 4") {
			val spellLvlT = extractSpellLevel(row)
			spellLvlT.get should be (4)
		}

	}

	describe("Persistent AoE function") {
		it("Will detect damage AoEs") {
			val lines = FileLocator.nssLines("nw_s0_inccloudc").toList
			extractIsPersistentAoE(lines) should be(true)
		}

		// the function makes no distinction between damage AoEs and non-damage,
		// therefore it should be able to handle non-damage persistent AoEs, for
		// re-usability purposes.
		it("Will detect non-damage") {
			val lines = FileLocator.nssLines("nw_s0_cloudkillc").toList
			extractIsPersistentAoE(lines) should be (true)
		}

		it("won't give false positives") {
			val lines = FileLocator.nssLines("cmi_s0_callfrost").toList
			extractIsPersistentAoE(lines) should be (false)
		}

	}

}
