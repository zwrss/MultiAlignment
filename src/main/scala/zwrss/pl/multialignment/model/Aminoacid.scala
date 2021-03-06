package zwrss.pl.multialignment.model

/**
 * Aminoacid.
 * Complete collection of compare methods from this trait's implementation represents Blosum62 matrix.
 */
abstract class Aminoacid(symbol: Char) {

  /**
   * Compares two Aminoacids using Blosum62.
   * @param that Aminoacid to be compared.
   * @return Score from Blosum62.
   */
  def compare(that: Aminoacid): Int = {
    if(internalCompare.isDefinedAt(that)) internalCompare.apply(that)
    else that.internalCompare.apply(this)
  }

  /**
   * Internal implementation of Blosum62.
   */
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int]

  /**
   * Returns one letter symbol of the Aminoacid.
   */
  def toChar: Char = symbol

}

object Aminoacid {
  def fromChar(c: Char): Aminoacid = c match {
    case 'a' => Ala
    case 'c' => Cys
    case 'd' => Asp
    case 'e' => Glu
    case 'f' => Phe
    case 'g' => Gly
    case 'h' => His
    case 'i' => Ile
    case 'k' => Lys
    case 'l' => Leu
    case 'm' => Met
    case 'n' => Asn
    case 'p' => Pro
    case 'q' => Gln
    case 'r' => Arg
    case 's' => Ser
    case 't' => Thr
    case 'v' => Val
    case 'w' => Trp
    case 'y' => Tyr
    case '.' => Empty
    case _ => throw new IllegalArgumentException(c + " does not represent any aminoacid!")
  }
}

case object Ala extends Aminoacid('a') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Ala) => 4
    case (Arg) => -1
    case (Asn) => -2
    case (Asp) => -2
    case (Cys) => 0
    case (Gln) => -1
    case (Glu) => -1
    case (Gly) => 0
    case (His) => -2
    case (Ile) => -1
    case (Leu) => -1
    case (Lys) => -1
    case (Met) => -1
    case (Phe) => -2
    case (Pro) => -1
    case (Ser) => 1
    case (Thr) => 0
    case (Trp) => -3
    case (Tyr) => -2
    case (Val) => 0
  }

}

case object Arg extends Aminoacid('r') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Arg) => 5
    case (Asn) => 0
    case (Asp) => -2
    case (Cys) => -3
    case (Gln) => 1
    case (Glu) => 0
    case (Gly) => -2
    case (His) => 0
    case (Ile) => -3
    case (Leu) => -2
    case (Lys) => 2
    case (Met) => -1
    case (Phe) => -3
    case (Pro) => -2
    case (Ser) => -1
    case (Thr) => -1
    case (Trp) => -3
    case (Tyr) => -2
    case (Val) => -3
  }
}

case object Asn extends Aminoacid('n') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Asn) => 6
    case (Asp) => 1
    case (Cys) => -1
    case (Gln) => 0
    case (Glu) => 0
    case (Gly) => 0
    case (His) => 1
    case (Ile) => -3
    case (Leu) => -3
    case (Lys) => 0
    case (Met) => -2
    case (Phe) => -3
    case (Pro) => -2
    case (Ser) => 1
    case (Thr) => 0
    case (Trp) => -4
    case (Tyr) => -2
    case (Val) => -3
  }
}

case object Asp extends Aminoacid('d') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Asp) => 6
    case (Cys) => -3
    case (Gln) => 0
    case (Glu) => 2
    case (Gly) => -1
    case (His) => -1
    case (Ile) => -3
    case (Leu) => -4
    case (Lys) => -1
    case (Met) => -3
    case (Phe) => -3
    case (Pro) => -1
    case (Ser) => 0
    case (Thr) => -1
    case (Trp) => -4
    case (Tyr) => -3
    case (Val) => -3
  }
}

case object Cys extends Aminoacid('c') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Cys) => 9
    case (Gln) => -3
    case (Glu) => -4
    case (Gly) => -3
    case (His) => -3
    case (Ile) => -1
    case (Leu) => -1
    case (Lys) => -3
    case (Met) => -1
    case (Phe) => -2
    case (Pro) => -3
    case (Ser) => -1
    case (Thr) => -1
    case (Trp) => -2
    case (Tyr) => -2
    case (Val) => -1
  }
}

case object Gln extends Aminoacid('q') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Gln) => 5
    case (Glu) => 2
    case (Gly) => -2
    case (His) => 0
    case (Ile) => -3
    case (Leu) => -2
    case (Lys) => 1
    case (Met) => 0
    case (Phe) => -3
    case (Pro) => -1
    case (Ser) => 0
    case (Thr) => -1
    case (Trp) => -2
    case (Tyr) => -1
    case (Val) => -2
  }
}

case object Glu extends Aminoacid('e') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Glu) => 5
    case (Gly) => -2
    case (His) => 0
    case (Ile) => -3
    case (Leu) => -3
    case (Lys) => 1
    case (Met) => -2
    case (Phe) => -3
    case (Pro) => -1
    case (Ser) => 0
    case (Thr) => -1
    case (Trp) => -3
    case (Tyr) => -2
    case (Val) => -2
  }
}

case object Gly extends Aminoacid('g') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Gly) => 6
    case (His) => -2
    case (Ile) => -4
    case (Leu) => -4
    case (Lys) => -2
    case (Met) => -3
    case (Phe) => -3
    case (Pro) => -2
    case (Ser) => 0
    case (Thr) => -2
    case (Trp) => -2
    case (Tyr) => -3
    case (Val) => -3
  }
}

case object His extends Aminoacid('h') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (His) => 8
    case (Ile) => -3
    case (Leu) => -3
    case (Lys) => -1
    case (Met) => -2
    case (Phe) => -1
    case (Pro) => -2
    case (Ser) => -1
    case (Thr) => -2
    case (Trp) => -2
    case (Tyr) => 2
    case (Val) => -3
  }
}

case object Ile extends Aminoacid('i') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Ile) => 4
    case (Leu) => 2
    case (Lys) => -3
    case (Met) => 1
    case (Phe) => 0
    case (Pro) => -3
    case (Ser) => -2
    case (Thr) => -1
    case (Trp) => -3
    case (Tyr) => -1
    case (Val) => -3
  }
}

case object Leu extends Aminoacid('l') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Leu) => 4
    case (Lys) => -2
    case (Met) => 2
    case (Phe) => 0
    case (Pro) => -3
    case (Ser) => -2
    case (Thr) => -1
    case (Trp) => -2
    case (Tyr) => -1
    case (Val) => 1
  }
}

case object Lys extends Aminoacid('k') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Lys) => 5
    case (Met) => -1
    case (Phe) => -3
    case (Pro) => -1
    case (Ser) => 0
    case (Thr) => -1
    case (Trp) => -3
    case (Tyr) => -2
    case (Val) => -2
  }
}

case object Met extends Aminoacid('m') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Met) => 5
    case (Phe) => 0
    case (Pro) => -2
    case (Ser) => -1
    case (Thr) => -1
    case (Trp) => -1
    case (Tyr) => -1
    case (Val) => 1
  }
}

case object Phe extends Aminoacid('f') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Phe) => 6
    case (Pro) => -4
    case (Ser) => -2
    case (Thr) => -2
    case (Trp) => 1
    case (Tyr) => 3
    case (Val) => -1
  }
}

case object Pro extends Aminoacid('p') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Pro) => 7
    case (Ser) => -1
    case (Thr) => -1
    case (Trp) => -4
    case (Tyr) => -3
    case (Val) => -2
  }
}

case object Ser extends Aminoacid('s') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Ser) => 4
    case (Thr) => 1
    case (Trp) => -3
    case (Tyr) => -2
    case (Val) => -2
  }
}

case object Thr extends Aminoacid('t') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Thr) => 5
    case (Trp) => -2
    case (Tyr) => -2
    case (Val) => 0
  }
}

case object Trp extends Aminoacid('w') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Trp) => 11
    case (Tyr) => 2
    case (Val) => -3
  }
}

case object Tyr extends Aminoacid('y') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Tyr) => 7
    case (Val) => -1
  }
}

case object Val extends Aminoacid('v') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case (Val) => 4
  }
}

case object Empty extends Aminoacid('.') {
  protected[model] def internalCompare: PartialFunction[Aminoacid, Int] = {
    case a: Aminoacid => 0
  }
}
