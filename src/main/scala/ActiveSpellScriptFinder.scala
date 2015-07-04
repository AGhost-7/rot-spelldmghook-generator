import java.io.File



/** Trait contains functions used to list all spells scripts that PCs can take
	* in the game. There are several scripts which are either monster-only or
	* legacy code from nwn1, and this filters it out.
	*/
trait ActiveSpellScriptFinder {

	/** This is a list of all class feats in the game, no duplicates or anything
		* here, only id values.
		*/
	def allClassFeats: Seq[String] = {
		val classes2da = TwoDAParser.parseFile("classes")

		classes2da.flatMap { row =>
			val lw = row("FeatsTable").toLowerCase
			if (lw != "****" && row("PlayerClass") == "1") {
				Some(lw)
			} else {
				None
			}
		}.flatMap { file: String =>
			val rows = TwoDAParser.parseFile(file)
			rows.map { row => row("FeatIndex") }
		}.distinct
	}

	/** If the spell is tied to a feat instead of direct spellcasting, I want to
		* verify that the feat is actually not removed, or simply a dangling feat,
		* with no possibility of a player ever taking it.
		*/
	def isDeadFeat(featId: String, classFeats: Seq[String]): Boolean = {
		Feat2da.all.find { _("id") == featId }.map { featRow =>
			if(featRow("REMOVED") == "1") {
				true
			} else {
				if(featRow("ALLCLASSESCANUSE") == "0") {
					// Check if the feat is unaccessible by any class? If its not a
					// general feat and theres no class with the flag needed to take the
					// feat then I can count the spell as being dead.
					!classFeats.exists { _ == featId }
				} else {
					false
				}
			}
		}.getOrElse(true)
	}

	/** Returns a list of spell scripts which are still being used in the game.
		* Script names are automatically lower cased.
		* @return Tuple of a string which is the lowercase version of the script name
		* along with a map which is the row from the 2da file.
		*/
	def findAllActiveSpellScripts: Seq[(String, Map[String, String])] = {
		// I only might need to load the 2da data for class feats, and I only need
		// it for the execution of this function.
		lazy val classFeats = allClassFeats

		val casters = List(
			"Bard", "Cleric", "Druid",
			"Paladin", "Ranger", "Wiz_Sorc",
			"Warlock")
		//println("Spells: " + Spells2da.all.size)
		Spells2da.all.map { row =>
			//println(s"id: ${row("id")}, script: ${row("ImpactScript")}, removed: ${row("REMOVED")}")
			val script = row("ImpactScript").toLowerCase

			if(row("REMOVED") == "1") {
				//println("removed: " + script)
				None
			} else if(script == "****") {
				None
			} else if(row("FeatID") != "****") {
				if(isDeadFeat(row("FeatID"), classFeats)) {
					None
				} else {
					Some((script, row))
				}
				// Check if its a spell in the game that can actually be taken by PCs.
			} else if(casters.forall { colName => row(colName) == "****"}) {
				None
			} else {
				Some((script, row))
			}
		}.flatten

	}

}