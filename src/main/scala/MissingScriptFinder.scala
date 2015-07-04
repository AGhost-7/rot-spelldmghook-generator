

object MissingScriptFinder extends ActiveSpellScriptFinder {

	/** Make 100% sure we got all script files from the stock game to override.
		* Returns a list of spell scripts which are still being used that aren't in
		* the campaign folder.
		*/
	def findMissingNssFiles: Seq[String] =
		findAllActiveSpellScripts
			.map { _._1 }
			.filter { script =>
				FileLocator.nssOpt(script) match {
					case None => true
					case _: Some[_] => false
				}
			}


	def main(args: Array[String]): Unit = {
		val missing = findMissingNssFiles
		missing.seq.foreach(println)
		println("Total missing scripts: " + missing.length)
	}
}
