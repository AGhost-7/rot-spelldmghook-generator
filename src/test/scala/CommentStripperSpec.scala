
import org.scalatest._


class CommentStripperSpec extends FlatSpec with ShouldMatchers {
	val cmntStripper = new CommentStripper {}
	import cmntStripper._

	val lines = List(
		"void main()// foobar!",
		"{",
		"}",
		"int MkDecision(int Hour/*I need the time of day to make a decision*/)",
		"{",
		"\treturn Random(5);",
		"}",
		"/*",
		"I am a multiline comment!",
		"ajsdkndkgfnrggdmasldkmn",
		"*/"
	)

	val stripped = stripOfComments(lines)

	it should "keep the code intact" in {
		stripped(0) should startWith("void main")
	}

	it should "be able to remove regular \"//\" comments" in {
		stripped.exists { _.contains("foobar!") } should be (false)
	}

	it should "be able to remove tricky /**/ comments" in {
		stripped.exists { _.contains("I need the time of day") } should be (false)
	}

	// ALWAYS test against real scripts just in case
	it should "be able to strip the comments from the icestorm spell script" in {
		val icestorm = FileLocator.nssLines("nw_s0_icestorm").toList
		val strp = stripOfComments(icestorm)
		strp should not contain("Ice")
		strp should not contain("Preston Watamaniuk")
		strp should not contain("AGhost_7")
	}
}
