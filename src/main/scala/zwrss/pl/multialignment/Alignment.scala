package zwrss.pl.multialignment

import scala.util.Random

case class Alignment(sequences: Seq[Sequence]) {

  if(sequences.exists(_.bases.length != length)) sys.error("ZLA DLUGOSC")

  override def toString(): String = {
    sequences.mkString("\n")
  }

  lazy val length: Int = sequences.head.bases.length

  def withRemovedSpaces: Alignment = {
    val trimmedSequences = (0 to (length - 1)).foldLeft(sequences) {
      (sequencesBuffer, index) =>
        if(index < sequencesBuffer.head.bases.length && sequencesBuffer.forall(_.checkForSpace(index))) sequencesBuffer.map(_.withRemovedSpace(index))
        else sequencesBuffer
    }
    new Alignment(trimmedSequences)
  }

  lazy val score: Int = {
    (0 to (length - 1)).foldLeft(0) {
      (score, i) =>
        (0 to (sequences.length - 1)).foldLeft(score) {
          (score, j) => ((j + 1) to (sequences.length - 1)).foldLeft(score) {
            (score, k) => score + sequences(j).apply(i).compare(sequences(k).apply(i))
          }
        }
    }
  }

  def compare(that: Alignment): Int = this.score - that.score

  def improved: Alignment = {
    val max = 10000
    var x = 0
    var r = this
    while(x < max) {
      if(mutated.compare(this) > 0) {
        r = lastMutated
        x = max
//        println("---")
//        println(this)
//        println(lastMutated)
      }
      x += 1
    }
    r
  }

  var lastMutated: Alignment = this

  def mutated: Alignment = {
    val sidx = Random.nextInt(sequences.length)
    val x1 = Random.nextInt(sequences(sidx).bases.length)
    val x2 = Random.nextInt(sequences(sidx).bases.length)
    var id = 0
    lastMutated = new Alignment(sequences.map(s => {
      val r = if(id == sidx) s.swap(x1, x2) else s
      id += 1
      r
    }))
    lastMutated
  }
}

object AlignmentFactory {
  def getSimpleAlignment(rawSequences: Seq[Sequence], length: Int) = {
    val max: Int = length //Helper.maxGT0(rawSequences.map(_.bases.length))
    val seqs: Seq[Sequence] = rawSequences.map(_.filledWithSpaces(max))
    new Alignment(seqs)
  }

  def getRandomAlignment(rawSequences: Seq[Sequence], length: Int): Alignment = {
    new Alignment(rawSequences.map(_.filledWithRandomSpaces(length)))
  }
}
