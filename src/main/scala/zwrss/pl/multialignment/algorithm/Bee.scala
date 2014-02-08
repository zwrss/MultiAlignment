package zwrss.pl.multialignment.algorithm

import zwrss.pl.multialignment.model.{Sequence, Alignment}
import scala.util.Random

/**
 * Bee :)
 */
case class Bee(solution: Alignment, originalSequences: Seq[Sequence], hardWorking: Int) {

  /**
   * Work it!
   */
  def work: Bee =
    Bee(solution.findBetterNeighbour(hardWorking).stripEmpties, originalSequences, hardWorking)

  /**
   * Get new random alignment!
   */
  def scout: Bee =
    Bee(Alignment.randomExtended(originalSequences, Random.nextInt(hardWorking)), originalSequences, hardWorking)

  /**
   * Score for sorting.
   */
  def score: Int = solution.score
}
