
import org.scalatest._

class TwoDAParserSpec extends FlatSpec with ShouldMatchers {
	val parser = new TwoDAParser {}
	import parser._

	def expected2daPattern(checks: Seq[Seq[(String, String)]])
			(lines: Seq[String]) {
		val rows = parseLines(lines)
		for {
			(rowCheck, i) <- checks.zipWithIndex
			(key, value) <- rowCheck
		} rows(i)(key) should equal(value)
	}

	val checks = Seq(
		Seq("id" -> "0", "name" -> "helloworld"))
	val isParsable = expected2daPattern(checks) _

	val header = Seq("2DA\tV2.0\t\t\t\t", "")

	it should "be able to handle basic space-based 2das" in {
		isParsable(
			header ++ Seq(
				"    name",
				"0   helloworld"))
	}

	it should "be able to handle basic tab-based 2das" in {
		isParsable(
			header ++Seq(
				"\tname\tfoo",
				"0\thelloworld\tfoo"))
	}

	it should "be able to handle 2das with post-spaces" in {
		isParsable(
			header ++ Seq(
				"   name",
				"0  helloworld "))
	}

	it should "be able to handle 2das with post-tabs" in {
		isParsable(
			header ++ Seq(
				"\tname",
				"0\thelloworld\t"))
	}

	it should "be able to parse more complicated 2das" in {
		val lines = Seq(
			"    name    age ",
			"0   foobar  5   ",
			"1   ghost   100 "
		)
		val cols = spacedRowColumns(lines(0))
		val p = parseSpacedRow(cols) _
		val rows = lines.drop(1).map(p)

		cols(0)._1 should be("id")
		cols(0)._2 should be(0)
		cols(1)._2 should be(4)
		rows(0)("name") should be("foobar")
		rows(0)("age") should be("5")
		rows(1)("id") should be("1")
		rows(1)("name") should be("ghost")
	}

	it should "be able to get the right data for icestorm" in {
		val rows = parseFile("spells")

		val icestorm = rows.find { _("Label") == "Ice_Storm" }.get

		icestorm("ImpactScript") should equal ("NW_S0_IceStorm")
		// V for evocation, since E is enchantment
		icestorm("School") should equal ("V")
		icestorm("Wiz_Sorc") should equal ("4")
	}

	it should "be able to parse the monk feat 2da" in {
		val rows = parseFile("cls_feat_monk")
		rows(0)("FeatLabel") should equal("WeapProfMonk")
		rows(5)("id") should equal("5")
	}
}
