package zwrss.pl.multialignment.algorithm

import zwrss.pl.multialignment.model.{Alignment, Sequence}

/**
 * Bee swarm algorithm.
 */
class BeeSwarm(sequences: Seq[Sequence], scoutsNo: Int, workersNo: Int, hardWorking: Int) {

  if(workersNo >= scoutsNo) throw new IllegalArgumentException("There should be more scouts than workers!")

  var bees: Seq[Bee] = (1 to scoutsNo).map(_ => Bee(Alignment.create(sequences), sequences, hardWorking))

  /**
   * Performs one iteration of the algorithm
   */
  def iterate: Unit = {
    val (workers, scouts) = bees.sortBy(-_.score).splitAt(workersNo)
    bees = workers.map(_.work) ++ scouts.map(_.scout)
  }

  /**
   * Performs n iterations of the algorithm.
   */
  def iterate(n: Int): Unit = (1 to n).foreach(_ => iterate)

  /**
   * Returns the best solution
   */
  def solution: Alignment = {
    bees = bees.sortBy(-_.score)
    bees.head.solution.stripEmpties
  }
}
