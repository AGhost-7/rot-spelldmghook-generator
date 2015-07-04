import constants._
import scala.util.matching.Regex._
import scala.collection.GenSeq
import scala.io.Source
import java.io.File

trait TwoDAParser {

	type RowName = String

	/** Row type is nothing more than a map, since each 2da will have different
		* fields, I can't use a proper class.
		*/
	type Row = Map[RowName, String]
	type StartIndex = Int

	val colPat = """([\t ]+)([A-z0-9*_]+)""".r

	val extractPat = """([A-z0-9*_]+)[\t ]{0,1}""".r

	/** This function extracts specifically the row column names along with their
		* starting index. The standard row number is given the "id" key.
		*/
	def spacedRowColumns(nameRow: String): Seq[(RowName, StartIndex)] = {
		def extract(matches: List[Match], iter: List[(RowName, StartIndex)]):
				List[(RowName, Int)] =
			matches match {
				case Nil => iter
				case m :: tail if iter.length == 1 =>
					extract(tail, (m.group(2), m.group(1).length) :: iter)
				case m :: tail =>
					// the starting index is the sum of the previous start index and the
					// length of the spaces added.
					val i: Int = iter.head._2 + iter.head._1.length + m.group(1).length
					extract(tail, (m.group(2), i) :: iter)
			}

		val matches = colPat.findAllMatchIn(nameRow).toList

		extract(matches, List("id" -> 0)).reverse
	}

	def parseSpacedRow(cols: Seq[(RowName, StartIndex)])(row: String): Row =
		cols.map { case (rowName, startIndex) =>
			val sub = row.substring(startIndex)
			val endIndex = sub.indexOf(" ")

			val value = try { extractPat.findFirstMatchIn(sub).get.group(1) }
			catch {
				case err: Throwable =>
					println("row: " + row)
					println("startIndex: " + startIndex)
					println("sub: " + sub)
					println("endIndex: " + endIndex)
					throw err
			}

			rowName -> value
		}.toMap

	def tabedRowColumns(row: String): Seq[String] =
		"id" +: row.tail.split("\t").toSeq

	def parseTabedRow(cols: Seq[String])(row: String): Row =
		cols.zip(row.split("\t")).toMap


	/** Parses all lines in the 2da. Needs the entire file contents. The algorithm
		* is able to detect the two very different formats we see in 2das.
		*/
	def parseLines(lines: Seq[String]): Seq[Row] = {
		val colLine = lines(2)
		val parser = if(colLine(0) == ' ') {
			val cols = spacedRowColumns(colLine)
			parseSpacedRow(cols) _
		} else {
			val cols = tabedRowColumns(colLine)
			parseTabedRow(cols) _
		}
		lines.drop(3).map(parser)
	}

	def lines(filename: String): Seq[String] =
		FileLocator.twoDALines(filename).toSeq

	def parseFile(file: String): Seq[Row] = {
		parseLines(lines(file))
	}

	def parseFile(file: File): Seq[Row] = parseFile(file.getAbsolutePath)

}

object TwoDAParser extends TwoDAParser

class Global2da(val fileName: String) extends TwoDAParser {
	lazy val lines: Seq[String] = lines(fileName)
	lazy val rows = lines.drop(3)
	lazy val all: Seq[Row] = parseLines(lines)
}

object Feat2da extends Global2da("feat")

object Spells2da extends Global2da("spells") {


	// spell school must be extracted from the 2da files.
	type ScriptName = String
	type SchoolConst = String
	lazy val map: Map[ScriptName, SchoolConst] = rows
		.par
		.map { line =>
			val sub = line.drop(91)
			val script = sub.drop(39).takeWhile{ _ != ' ' }.toLowerCase
			val schoolConst = spellSchoolCodes(sub(0))
			script -> schoolConst
		}
		.toMap
		.seq
}
