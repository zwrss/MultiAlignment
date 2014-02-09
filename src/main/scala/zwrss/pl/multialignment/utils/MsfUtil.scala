package zwrss.pl.multialignment.utils

import java.util.Scanner
import zwrss.pl.multialignment.model.Sequence
import java.io.{StringWriter, FileWriter, BufferedWriter, File}

/**
 * Use this class to extract aminoacid sequences from file.
 * Only msf format supported.
 */
case class MsfUtil(filename: String) { //TODO Smarter mechanism - with headers handling.

  private lazy val lines = {
    var lns = Seq.empty[String]
    val scanner = new Scanner(new File(filename))
    while(scanner.hasNextLine) lns = lns :+ scanner.nextLine
    scanner.close()
    lns
  }

  /**
   * Returns sequences from file.
   */
  def getSequences: Seq[Sequence] = {

    var map = Map.empty[String, String]
    val scanner = new Scanner(new File(filename))
    var alignmentFlag = false

    while(scanner.hasNextLine) {
      val line = scanner.nextLine()
      if(alignmentFlag && !line.replaceAll("[ \t]", "").isEmpty) {
        val array = line.split("[ \t]").filterNot(_.isEmpty)
        val name = array.head
        val seq = (map.get(name).toSeq ++ array.tail).mkString
        map = map.updated(name, seq)
      }
      else if(line.replaceAll("[ \t]", "") == "//") alignmentFlag = true
    }

    map.map{
      case (name, seq) => Sequence.fromString(name, seq)
    }.toSeq
  }

  /**
   * Returns sequences without empty spaces.
   */
  def getStrippedSequences: Seq[Sequence] = getSequences.map(_.removedAllEmpties)

  /**
   * Saves provided sequences.
   */
  def save(sequences: Seq[Sequence]) {
    val newFilename = filename.stripSuffix(".msf") + "_modified.msf"
    val writer = new BufferedWriter(new FileWriter(new File(newFilename)))

    //Close header
    writer.write("//")
    writer.newLine()
    writer.newLine()
    writer.newLine()
    writer.newLine()

    //Write sequences
    sequences.map(s => {
      writer.write(s.string)
      writer.newLine()
    })

    writer.newLine()
    writer.newLine()
    writer.newLine()
    writer.close()
  }

}
