import java.io.File
import scala.collection.mutable.ListBuffer
import pipeOps._

import scala.util.matching.Regex.Match

object SpellWriter {

	def writeStructs(schoolConst: String,
			damageTypes: List[String],
			isAoE: Boolean): List[String] = {

		val aoeStr = if(isAoE) "TRUE" else "FALSE"

		"" :: "\t// AGhost_7 :: for details see inc_rot_hooks_spelldmg" ::
			damageTypes.zipWithIndex.map { case (damageType, i) =>
				val varName = if (i == 0) "structBonusDmg" else "structBonusDmg" + (i + 1)
				s"""\tstruct BonusSpellDmg $varName = GetBonusSpellDamage(OBJECT_SELF, $schoolConst, $damageType, $isAoE);"""
			}
	}

	def structLocation() = ???

	/** Might need to add the include file for the hook, for the most part I think
		* the shell script took care of it, but just in case...
		*/
	def addInclude(lines: List[String]): List[String] = {
		if(lines.forall { !_.contains("inc_rot_hooks_spelldmg") }) {

			val index = lines.indexWhere { _.contains("#include") }
			lines.take(index) ++
				List("""#include "inc_rot_hooks_spelldmg" """) ++
				lines.drop(index)
		} else {
			lines
		}
	}

	val whilePat = """(\s*)(while\s*\(\s*GetIsObjectValid)""".r

	/** Find where to place the struct declarations in the file. */
	def findStructIndex(lines: List[String], isAoE: Boolean): Int = {
		val hookStart = lines.indexWhere { _.contains("X2PreSpellCastCode()") }
		val checkLines = lines.drop(hookStart)

		val cmt = checkLines.indexWhere { _.contains("End of Spell Cast Hook") }
		val part = if(cmt > -1) {
			cmt
		} else {
			// if there is a closing bracket before an opening one, I can assert
			// that its for closing the spell cast hook return statement.
			val rt = checkLines.indexWhere { _.contains("}") }
			if(rt > -1) rt
			else checkLines.indexWhere { _.contains("return") }
		}
		hookStart + part + 1
	}


	/** Takes the lines given and will insert at the proper location the struct
		* declaration, returning the result list of lines.
		*/
	def addStructs(school: String, types: List[String], isAoE: Boolean)
			(lines: List[String]): List[String] = {

		val structIndex = findStructIndex(lines, isAoE)
		val structs = writeStructs(school, types, isAoE)

		lines.take(structIndex) ++
			structs ++
			lines.drop(structIndex)
	}



	def writeHookedSpell(hspell: HookedSpell): Unit = {
		val HookedSpell(mem, school, types, level, isAoE) = hspell
		val MemFile(file, row, lines) = mem

		lines |> addInclude |> addStructs(school, types, isAoE) |> writeFile(file)
	}


	def writeFile(file: File)(lines: List[String]): Unit = {
		import java.io._
		val outFile = new File(FileLocator.root + "hook-generator/output/" + file.getName)

		if(!outFile.exists()) outFile.createNewFile()

		val writer = new PrintWriter(new FileOutputStream(file, false))

		val output = lines.mkString("\n")
		lines.foreach(writer.println)

		writer.close()
	}


	def main(arg: Array[String]): Unit = {

		// start by generating the output directory...
		new File("output").mkdirs()
		val nssFiles = SpellExtractor.activeNssSpellFiles
		val (hSpells, errs) = SpellExtractor.getTargetScripts(nssFiles)

		println("Number of spells outputed: " + hSpells.length)

		hSpells.foreach(writeHookedSpell)


	}

}
