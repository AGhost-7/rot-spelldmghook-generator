import java.io.File
import io.Source

/** I'm using this so I can avoid hardcoding file paths all over the place. */
object FileLocator {

	val root = "../rotdev/"
	val twoDARoot = "2da/"
	val nssFileRoot = "campaigns/Realms of Trinity/"

	lazy val directory = file(root)
	lazy val twoDAFiles = listOfExt(root + twoDARoot, ".2da")
	lazy val nssFiles = listOfExt(root + nssFileRoot, ".nss")

	/** I'm going to cache the file list to keep this speedy. */
	private def listOfExt(dir: String, ext: String): Map[String, File] = new File(dir)
			.listFiles()
			.filter { _.getName.toLowerCase.endsWith(ext) }
			.map { fl => fl.getName.toLowerCase.replaceAll(ext, "") -> fl }
			.toMap

	private def searcherOpt(dirMap: Map[String, File], ext: String): (String => Option[File]) = {
		(filename) =>
			val lowered = filename.toLowerCase
			dirMap
				.get(lowered) match {
					case s @ Some(f) => s
					case None => dirMap
						.get(lowered.replaceAll(ext, ""))
				}
	}

	private def searcher(dirMap: Map[String, File], ext: String): (String => File) = {
		val searchFun = searcherOpt(dirMap, ext)
		(filename) =>
			searchFun(filename)
				.getOrElse { throw new NoSuchElementException(s"Could not find $ext file: " + filename) }
	}

	val twoDAOpt = searcherOpt(twoDAFiles, ".2da")
	val nssOpt = searcherOpt(nssFiles, ".nss")

	val twoDA = searcher(twoDAFiles, ".2da")
	val nss = searcher(nssFiles, ".nss")

	def twoDALines(filename: String): Iterator[String] =
		Source.fromFile(twoDA(filename)).getLines

	def nssLines(filename: String): Iterator[String] =
		Source.fromFile(nss(filename)).getLines

}
