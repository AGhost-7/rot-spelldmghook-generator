import java.io.File

import scala.collection.parallel.ParSeq
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Failure, Try, Success}

import constants._
import pipeOps._

abstract class Panic extends Throwable {
	def file: File
	def msg: String
	def print : Unit = println(file.getName + " - " + msg)
}

case class ScriptNotFound(file: File, scriptName: String) extends Panic {
	val msg = "Could not find in 2da corresponding script name: " + scriptName
}
case class InvalidDamageType(file: File, errType: String) extends Panic {
	val msg = "Damage type " + errType + " is not valid"
}

/** Nothing more than the file loaded into memory. */
case class MemFile(file: File, row: Map[String, String],  lines: List[String])
case class HookedSpell(

		mem: MemFile,
		// constant name, so that the code output is easy to read
		schoolConst: String,
		// these can either be a int literal or constant
		damageTypes: List[String],
		spellLevel: Int,
		isAoE: Boolean) {
	override def toString = s"${mem.file.getName} - $schoolConst - " +
		damageTypes.mkString(", ")
}

object SpellExtractor extends ActiveSpellScriptFinder with CommentStripper {

	type Row = Map[String, String]

	/** Load into memory the files, and prep for parsing. */
	def activeNssSpellFiles: ParSeq[Try[MemFile]] = findAllActiveSpellScripts
		.par
		.filter { _._2("FeatID") == "****" }
		.map { case (script, row) =>
			Try {
				val file = FileLocator
					.nssOpt(script)
					.getOrElse {
						val fl = new File(FileLocator.nssFileRoot + script + ".NSS")
						throw ScriptNotFound(fl, script)
					}
				val lines =
					Source.fromFile(file).getLines.toList// |> stripOfComments
				MemFile(file, row, lines)
			}
		}

	def extractIsPersistentAoE(lines: List[String]): Boolean = lines.exists { line =>
		line.contains("GetAreaOfEffectCreator()")
	}

	val variablePat = """^\s*(int|string|object|itemproperty|effect|)(\s+)([A-z09_]+)(\s*=\s*)([A-z0-9_]+)""".r

	/** Goes through the entire file to find what the variable was assigned. This
		* function won't account for re-assignments.
		*/
	def findVariableValue(varName: String, lines: List[String]): Option[String] = {
		def searchLines(lines: List[String]): Option[String] = lines match {
			case Nil => None
			case head :: rest =>
				variablePat
					.findFirstMatchIn(head)
					.fold {
						searchLines(rest)
					} { m =>
						val varNameFound = m.group(3)
						val varVal = m.group(5)

						if(varName == varNameFound) Some(varVal)
						else searchLines(rest)
				}
		}

		searchLines(lines.toList)
	}

	/** Some spells such as glaive and flame arrow aren't AoEs, but they're still
		* getting a significant bang for buck for each bit of damage since they have
		* more than one "hit".
		*/
//	def extractIsMultiHit(lines: List[String]): Boolean = {
//		// I need to find EffectDamage either inside of a for, while or recursive
//		// call, and I also need to make sure those call aren't for the
//		def extract(lines: List[String], depth: Int): Boolean = lines match {
//			case Nil =>
//				???
//			case head :: rest =>
//				???
//		}
//		// due to the complexity of the algorithm, I need to remove comments for
//		// read operations, but
//		extract(lines |> stripOfComments, 0)
//		???
//	}

	/** For now, just extract the innate level, don't want to bother with it
		* more than that.
		*/
	def extractSpellLevel(row: Row): Try[Int] = Try { row("Innate").toInt }

	val effectDamagePat = """(EffectDamage\([A-z0-9_()]+,\s*)([A-z0-9_]+)""".r

	/** Go through entire file once to find if EffectDamage is contained within
	 *  the file and find the damage type constant I need.
	 */
	def extractDamageTypes(file: File, lines: List[String]): Try[List[String]] =
		Try {
			def isDamageType(str: String): Boolean =
				str.forall { _.isDigit } || damageTypeConsts.exists { _ == str }
			lines.foldLeft(Nil: List[String]) { (accu, line) =>
				effectDamagePat.findFirstMatchIn(line) match {
					case Some(mat) =>
						val str = mat.group(2)
						if(isDamageType(str)) {
							str :: accu
						} else {
							val value = findVariableValue(str, lines)
								.getOrElse { throw InvalidDamageType(file, str) }
							value :: accu
						}
					case None =>
						accu
				}
			}.distinct
		}

	/** This will attempt to figure out if the spell is an AoE. AoE spells are
		* meant to deal less damage than single target spells such as orbs; I
		* not only need this to figure out where I'm going to insert the two
		* function calls, but also to find out what the function arguments are going
		* to be.
		*/
	def extractIsAoE(lines: List[String]): Boolean = {

		val pat = """while\s*\(GetIsObjectValid""".r

		def extract(ls: List[String],
				shapeGetter: Boolean,
				whileLoop: Boolean,
				nextShape: Boolean): Boolean = ls match {
			case Nil =>
				shapeGetter && whileLoop && nextShape
			case line :: rest if !shapeGetter && line.contains("GetFirstObjectInShape") =>
				extract(rest, true, whileLoop, nextShape)
			case line :: rest if !whileLoop && pat.findFirstIn(line).isDefined =>
				extract(rest, shapeGetter, true, nextShape)
			case line :: rest if !nextShape && line.contains("GetNextObjectInShape") =>
				extract(rest, shapeGetter, whileLoop, true)
			case line :: rest =>
				extract(rest, shapeGetter, whileLoop, nextShape)
		}

		extract(lines, false, false, false)
	}

	/** I think I'm going to want to return the constant value so its easier to
		* read the output, instead of using an integer literal.
		*/
	def find2daSchool(row: Row): Try[String] = Try {
		row("School") match {
			case "C" => "SPELL_SCHOOL_CONJURATION"
			case "G" | "****" => "SPELL_SCHOOL_GENERAL"
			case "A" => "SPELL_SCHOOL_ABJURATION"
			case "D" => "SPELL_SCHOOL_DIVINATION"
			case "E" => "SPELL_SCHOOL_ENCHANTMENT"
			case "V" => "SPELL_SCHOOL_EVOCATION"
			case "I" => "SPELL_SCHOOL_ILLUSION"
			case "N" => "SPELL_SCHOOL_NECROMANCY"
			case "T" => "SPELL_SCHOOL_TRANSMUTATION"
		}
	}

	/** Returns a list of spells which are of interest for the damage hook. Some
		* spells such as persistent AoEs are deemed too complicated or too few to
		* be worth including in this static code generator.
		* @return A list of pre-parsed spells along with a list of errors.
		*/
	def getTargetScripts(scriptsT: ParSeq[Try[MemFile]]):
			(ParSeq[HookedSpell], ParSeq[Panic]) = scriptsT
		.filter {
			case Success(MemFile(_, row, _)) => row("HostileSetting") == "1"
			case _ => true
		}
		.map { case fl =>
			for {
				mem @ MemFile(file, row, lines) <- fl
				school <- find2daSchool(row)
				damageTypes <- extractDamageTypes(file, lines)
				spellLevel <- extractSpellLevel(row)
			} yield HookedSpell(mem, school, damageTypes, spellLevel, extractIsAoE(lines))
		}
		.filter { hspellT =>
			hspellT
				.map { hspell =>
					!hspell.damageTypes.isEmpty && !extractIsPersistentAoE(hspell.mem.lines)
				}
				.getOrElse(true)
		}
		.foldLeft((ParSeq.empty[HookedSpell], ParSeq.empty[Panic])) {
			case (accu, Failure(err: Panic)) => (accu._1, err +: accu._2)
			case (accu, Failure(err)) => throw err
			case (accu, Success(spl)) => (spl +: accu._1, accu._2)
		}


	def main(arg: Array[String]): Unit = {
		//println("active spell scripts: " + findAllActiveSpellScripts.length)
		val (hSpells, errs) = getTargetScripts(activeNssSpellFiles)
		println("Errors:")
		errs.foreach { _.print }
		val typeErrs = errs.filter {
			case err: InvalidDamageType => true
			case _ => false
		}

		typeErrs.foreach{ _.print }





		//println("files loaded: " + findAllActiveSpellScripts.length)
		println("total parsed spells: " + hSpells.length)

		println("total errors: " + errs.length)
		println("total damage type errors: " + typeErrs.length)

	}
}
