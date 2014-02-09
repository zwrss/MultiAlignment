package zwrss.pl.multialignment.model

import scala.util.Random


/**
 * Sequence of aminoacids.
 */
case class Sequence(name: String, aminoacids: Seq[Aminoacid]) {

  /**
   * Comapre with one sequence and return score.
   *
   * @param that Sequence to be compared to.
   * @return Score (using Blosum62).
   */
  def compare(that: Sequence): Int = aminoacids.zip(that.aminoacids).foldLeft(0) {
    case (acc, (a, b)) => acc + a.compare(b)
  }

  /**
   * Compare with multiple sequences.
   *
   * @param multi Sequences to be compared to.
   * @return Score (using Blosum62).
   */
  def compare(multi: Seq[Sequence]): Int = multi.foldLeft(0) {
    case (acc, s) => acc + compare(s)
  }

  /**
   * Complements sequence with empty spaces. Doesn't throw if actual size is greater then the one provided.
   * @param size Desired length of the sequence.
   * @return Complemented sequence.
   */
  def complement(size: Int): Sequence = {
    val empties = (1 to (size - aminoacids.size)).foldLeft(Seq.empty[Aminoacid]) {
      case (seq, i) => seq :+ Empty
    }
    Sequence(name, this.aminoacids ++ empties)
  }

  /**
   * Complements sequence with randomly distributed empty spaces.
   * Doesn't throw if actual size is greater then the one provided.
   * @param size Desired length of the sequence.
   * @return Complemented sequence.
   */
  def complementRandomly(size: Int): Sequence = {
    def internalComplement(size: Int, seq: Seq[Aminoacid]): Seq[Aminoacid] =
      if(seq.size >= size) seq
      else {
        val pivot = Random.nextInt(seq.size)
        val (head, tail) = seq.splitAt(pivot)
        val compSeq = (head :+ Empty) ++ tail
        internalComplement(size, compSeq)
      }
    Sequence(name, internalComplement(size, aminoacids))
  }

  /**
   * Checks if there is an empty space on the provided index.
   */
  def emptyAt(idx: Int): Boolean = aminoacids(idx) == Empty

  /**
   * Returns sequence with removed empty on the provided index.
   * Throws if its not empty on that place.
   */
  def removeEmpty(idx: Int): Sequence = {
    if(!emptyAt(idx)) throw new IllegalArgumentException("Not empty at idx = " + idx + "!")
    val (head, tail) = aminoacids.splitAt(idx)
    Sequence(name, head ++ tail.tail)
  }

  /**
   * Returns sequence with removed empty on the provided indexes.
   * Throws if its not empty on that place.
   */
  def removeEmpties(ids: Seq[Int]): Sequence = {
    ids.sortBy(-_).foldLeft(this){
      case (s, i) => s.removeEmpty(i)
    }
  }

  /**
   * Returns sequence without empty spaces.
   */
  def removedAllEmpties: Sequence = {
    Sequence(name, aminoacids.filterNot(_.isInstanceOf[Empty.type]))
  }

  def string: String = name + "      " + aminoacids.map(_.toChar).mkString("")

}

object Sequence {
  def fromString(name: String, str: String): Sequence = Sequence(name, str.map(c => Aminoacid.fromChar(c)))
}