package zwrss.pl.multialignment.model

import scala.util.Random


/**
 * Sequence of aminoacids.
 */
case class Sequence(aminoacids: Seq[Aminoacid]) {

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
    Sequence(this.aminoacids ++ empties)
  }

  /**
   * Complements sequence with randomly distributed empty spaces.
   * Doesn't throw if actual size is greater then the one provided.
   * @param size Desired length of the sequence.
   * @return Complemented sequence.
   */
  def complementRandomly(size: Int): Sequence = {
    val empties = (1 to (size - aminoacids.size)).foldLeft(Seq.empty[Aminoacid]) {
      case (seq, i) => {
        val pivot = Random.nextInt(aminoacids.size)
        val (head, tail) = seq.splitAt(pivot)
        (head :+ Empty) ++ tail
      }
    }
    Sequence(this.aminoacids ++ empties)
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
    Sequence(head ++ tail.tail)
  }

}