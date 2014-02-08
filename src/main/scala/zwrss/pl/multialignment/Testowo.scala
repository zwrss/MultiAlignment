package zwrss.pl.multialignment

import zwrss.pl.multialignment.algorithm.BeeSwarm
import zwrss.pl.multialignment.model._

object Testowo {
  def main(args: Array[String]) {

    val s1 = Sequence(Seq(Ala, Arg, Glu, Gln, Gly, Ser, Thr, Trp, Tyr))
    val s2 = Sequence(Seq(Tyr, Ser, Gly, Ser, Thr, Trp, Gln))
    val s3 = Sequence(Seq(Ala, Arg, Thr, Trp, Tyr))

    val swarm = new BeeSwarm(Seq(s1, s2, s3), 200, 30, 100)

    println(swarm.solution.score)
    println(swarm.solution.string)
    swarm.iterate(100)
    println(swarm.solution.score)
    println(swarm.solution.string)

  }
}
