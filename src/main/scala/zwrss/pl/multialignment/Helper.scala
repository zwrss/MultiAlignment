package zwrss.pl.multialignment

object Helper {
  def maxGT0(ints: Seq[Int]): Int = {
    ints.foldLeft(0){
      (max, i) => if(max > i) max else i
    }
  }

  def removeFromSeq[T](seq: Seq[T], idx: Int): Seq[T] = {
    val (a, b) = seq.splitAt(idx)
    a ++ b.tail
  }
}
