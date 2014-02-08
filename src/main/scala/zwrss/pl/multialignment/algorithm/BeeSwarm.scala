package zwrss.pl.multialignment.algorithm

import zwrss.pl.multialignment.model.Sequence

/**
 * Bee swarm algorithm.
 */
class BeeSwarm(sequences: Seq[Sequence], scouts: Int, workers: Int) {

  if(workers >= scouts) throw new IllegalArgumentException("There should be more scouts than workers!")

  /**
   * Performs one iteration of the algorithm
   */
  def iterate {
    //TODO Iteration
  }

  /**
   * Performs n iterations of the algorithm.
   */
  def iterate(n: Int) = (1 to n).map(_ => iterate)
}
