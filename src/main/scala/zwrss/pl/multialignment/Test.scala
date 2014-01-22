package zwrss.pl.multialignment

object Test {
  def main(args: Array[String]) {
//    val s1 = Sequence(Seq(G, Space, A, Space, C, Space))
//    val s2 = Sequence(Seq(Space, A, A, C, Space, Space))
//    val a1 = AlignmentFactory.getSimpleAlignment(Seq(s1, s2), 10)
//    val a2 = AlignmentFactory.getRandomAlignment(Seq(s1, s2), 10)
//    val p1 = Population(Seq(a1, a2), Seq(s1, s2), 10)
//    println(p1)
//    println(p1.solutions.map(_.score))
//    val p2 = p1.iterated
//    println(p2)
//    println(p2.solutions.map(_.score))
//    println(a1)
//    println(a1.improved)
    val s1 = Sequence(Seq(A, A, C, G, T, A, C, C))
    val s2 = Sequence(Seq(G, A, T, G, A, C, C, T))
    val s3 = Sequence(Seq(C, C, C, A, A, C, C, G))
    val s4 = Sequence(Seq(A, C, C, A, G, T, C, C))
    val s5 = Sequence(Seq(A, A, C, G, T, A, G, G))
    val rawSequences = Seq(s1, s2, s3, s4, s5)
    println(rawSequences.mkString("\n"))
    println("-------------")
    val as = (1 to 30).map(_ => AlignmentFactory.getRandomAlignment(rawSequences, 20))
    val p = Population(as, rawSequences, 20)
    val newp = (1 to 100).foldLeft(p) {
      (population, i) => population.iterated
    }
    println(newp.ordered.solutions.head.withRemovedSpaces)
  }
}


case class Population(solutions: Seq[Alignment], rawSequences: Seq[Sequence], length: Int) {
  def ordered: Population = new Population(solutions.sortBy(-_.score), rawSequences, length)
  def iterated: Population = {
    val sortedSolutions = solutions.sortBy(-_.score)
    val (bestSolutions, rest) = sortedSolutions.splitAt(2)
    val newSolutions = bestSolutions.map(_.improved) ++ rest.map(_ => AlignmentFactory.getRandomAlignment(rawSequences, length))
    new Population(newSolutions, rawSequences, length)
  }
}