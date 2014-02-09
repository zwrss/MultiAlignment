package zwrss.pl.multialignment.utils

import zwrss.pl.multialignment.algorithm.BeeSwarm

object Main {
  def main(args: Array[String]) {
    val filename: String =
      if(args.size > 0 && args(0) != null) args(0)
      else sys.error("Path to the file containing sequences should be provided!")

    val scoutsNo: Int =
      if(args.size > 1 && args(1) != null) args(1).toInt
      else 100

    val workersNo: Int =
      if(args.size > 2 && args(2) != null) args(2).toInt
      else 25

    val hardWorking: Int =
      if(args.size > 3 && args(3) != null) args(3).toInt
      else 100

    val iterations: Int =
      if(args.size > 4 && args(4) != null) args(4).toInt
      else 200

    val msf = MsfUtil(filename)

    val sequences = msf.getStrippedSequences

    println("Input sequences from file " + filename + ": \n")
    println(sequences.map(_.string).mkString("\n"))

    val swarm = new BeeSwarm(sequences, scoutsNo, workersNo, hardWorking)
    swarm.iterate(iterations)

    println("\n\n ------------------------------------ \n\nSolution after " + iterations + " iterations: \n")
    println(swarm.solution.sequences.map(_.string).mkString("\n"))

    msf.save(swarm.solution.sequences)

  }
}
