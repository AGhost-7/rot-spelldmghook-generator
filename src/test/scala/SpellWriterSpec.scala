
import org.scalatest._

class SpellWriterSpec extends FlatSpec with ShouldMatchers {
	import SpellWriter._

	val lines = List(
		"// this is a mock file!",
		"""#include "hello" """,
		"",
		"void main()",
		"{",
		" SendMessageToPC(OBJECT_SELF, \"hello!\");",
		" object oTarget = GetFirstObjectInShapeBlahblah",
		" while(GetIsObjectValid(oTarget))",
		" {",
		"  effect eDamFire = EffectDamage(100, DAMAGE_TYPE_FIRE);",
		"  effect eDamCold = EffectDamage(100, DAMAGE_TYPE_COLD);",
		"  oTarget = GetNextObjectInShapeBlahblah",
		" }",
		"}"
	)

	it should "insert include at appropriate line" in {
		addInclude(lines)(1) should include("hook")
	}

	it should "insert the structs at the appropriate location for AoEs" in {
		val icestorm = FileLocator.nssLines("nw_s0_icestorm").toList
		val types = List("DAMAGE_TYPE_COLD")
		val withStructs = addStructs("SCHOOL_EVOCATION", types, true)(icestorm)
		val insert = withStructs.indexWhere { _.contains("struct") }
		val whileLoc = withStructs.indexWhere { _.contains("while") }

		insert should be < whileLoc
	}

	it should "insert the structs at the appropriate location for single target spells" in {
		val scorch = FileLocator.nssLines("nx_s0_scorchingraysingle").toList
		val withStructs = addStructs("SCHOOL_EVOCATION", List("DAMAGE_TYPE_FIRE"), false)(scorch)
		val insert = withStructs.indexWhere { _.contains("struct") }
		val whileLoc = withStructs.indexWhere { _.contains("while") }

		insert should be < whileLoc
	}


}
