package zwrss.pl.multialignment

case class Sequence(bases: Seq[NucleotideBase]) {

  override def toString(): String = {
    bases.mkString(", ")
  }

  def apply(idx: Int): NucleotideBase = bases(idx)

  def filledWithSpaces(n: Int): Sequence = {
    if(n < bases.length) this
    else {
      val updatedBases = (1 to (n - bases.length)).foldLeft(bases) {
        (b, i) => b :+ Space
      }
      Sequence(updatedBases)
    }
  }

  def filledWithRandomSpaces(n: Int): Sequence = {
    if(n < bases.length) this
    else {
      val updatedBases = (1 to (n - bases.length)).foldLeft(bases) {
        (b, i) => {
          val (x, y) = b.splitAt(scala.util.Random.nextInt(bases.length))
          (x :+ Space) ++ y
        }
      }
      Sequence(updatedBases)
    }
  }

  def withRemovedSpace(i: Int): Sequence = {
    if(bases(i) == Space) Sequence(Helper.removeFromSeq(bases, i))
    else sys.error("Element " + i + " is not an empty space!")
  }

  def checkForSpace(i: Int): Boolean = if(i >= bases.length) true else bases(i) == Space

  def swap(x1: Int, x2: Int): Sequence = {
    if(x1 == x2) this
    else if(x2 < x1) swap(x2, x2)
    else {
      val v1 = bases.take(x1)
      val v2 = bases.drop(x1).head
      val v3 = bases.take(x2).drop(x1 + 1)
      val v4 = bases.drop(x2).head
      val v5 = bases.drop(x2).tail
      Sequence((v1 :+ v4) ++ (v3 :+ v2) ++ v5)
    }
  }

}
