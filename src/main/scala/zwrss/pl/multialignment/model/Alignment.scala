package zwrss.pl.multialignment.model


/**
 * Alignment of Sequences.
 */
case class Alignment protected[model](sequences: Seq[Sequence], length: Int) {

  if(sequences.exists(_.aminoacids.size != length))
    throw new IllegalArgumentException("Not every sequence in the Aligment is " + length + " elements long!")

  /**
   * Score of this alignment
   */
  lazy val score: Int = sequences.foldLeft(0){ // I fukin luv foldLeft
    case (acc, s) => acc + s.compare(sequences)
  }

  /**
   * Tries to find better alignment in neighbourhood using randomness. Returns self on no better alignments found.
   * Neighbourhood is defined as any alignment that differs with one added empty space (to every sequence).
   */
  def findBetterNeighbour: Alignment = {
    def tryToFind(i: Int): Alignment =
      if(i == 0) this
      else {
        val a = Alignment(sequences.map(_.complementRandomly(length+1)), length+1)
        if(a.score > this.score) a
        else tryToFind(i - 1)
      }
    tryToFind(25)
  }

  /**
   * Strips sequences from unnecessary empty spaces.
   */
  def stripEmpties: Alignment = {
    val emptiesIds = (0 to length - 1).filter(i => sequences.forall(_.emptyAt(i))).reverse
    val strippedSequences: Seq[Sequence] = emptiesIds.flatMap(i => sequences.map(_.removeEmpty(i)))
    Alignment(strippedSequences, length - emptiesIds.size)
  }

}

/**
 * Alignments factory
 */
object Alignment {

  /**
   * Creates alignment with Sequences complemented with empty spaces on the end.
   */
  def create(sequences: Seq[Sequence]): Alignment = {
    val max = sequences.map(_.aminoacids.size).reduce(_ max _)
    Alignment(sequences.map(_.complement(max)), max)
  }

  /**
   * Creates alignment with Sequences complemented randomly with empty spaces.
   */
  def random(sequences: Seq[Sequence]): Alignment = {
    val max = sequences.map(_.aminoacids.size).reduce(_ max _)
    Alignment(sequences.map(_.complementRandomly(max)), max)
  }

}