
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** This trait contains the stripOfComments function which will remove all
	* c-style comments from the lines given.
	*/
trait CommentStripper {

	private val regCommentPat = """\/\/.+""".r

	def stripOfComments(lines: List[String]): List[String] = {
		// start with the easy part, remove regular comments.
		val noReg = lines.map { regCommentPat.replaceAllIn(_, "") }

		// Multiline comments can span... multiple lines... so going through line by
		// line won't work here; map isn't going to cut it.
		//
		// ListBuffer appends at constant time, and there's no way around it for
		// now... Mutations are still localized to the function call, meaning this
		// is still 100% thread safe without any locking.
		@tailrec
		def strip(
				lines: List[String],
				outLines: ListBuffer[String],
				line: List[Char],
				lineOut: ListBuffer[Char],
				lastChar: Char,
				inComment: Boolean): List[String] = line match {
			// we're at the last line, so we've got our result.
			case Nil if lines.isEmpty =>
				outLines += lineOut.mkString
				outLines.toList.filterNot { _.isEmpty }
			// end of the line, so get the next line and push parsed line into the list
			// we're going to output
			case Nil =>
				outLines += lineOut.mkString
				strip(lines.tail, outLines, lines.head.toList, ListBuffer.empty, '\n', inComment)
			// this marks the end of the multiline comment
			case c :: chars if inComment && c == '/' && lastChar == '*' =>
				strip(lines, outLines, chars, lineOut, c, false)
			// inside comments, all characters in here shouldn't be outputed
			case c :: chars if inComment =>
				strip(lines, outLines, chars, lineOut, c, inComment)
			// comment section begins
			case c :: chars if c == '*' && lastChar == '/' =>
				strip(lines, outLines, chars, lineOut, c, true)
			// skip the starting character for the multiline comment
			case c :: chars if c == '/' =>
				strip(lines, outLines, chars, lineOut, c, inComment)
			// otherwise, we're seeing regular code output, no need to strip this out.
			case c :: chars =>
				lineOut += c
				strip(lines, outLines, chars, lineOut, c, inComment)
		}

		strip(noReg.tail, ListBuffer.empty, noReg.head.toList, ListBuffer.empty, 7.toChar, false)
	}
}
