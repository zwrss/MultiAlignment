package zwrss.pl.multialignment

sealed trait NucleotideBase {
  def compare(that: NucleotideBase): Int = {
    val v = if(this == Space || that == Space) 0
    else if(this == that) 10
    else -5
//    println(this + " ? " + that + " = " + v)
    v
  }
}

case object G extends NucleotideBase
case object A extends NucleotideBase
case object T extends NucleotideBase
case object C extends NucleotideBase

case object Space extends NucleotideBase {
  override def toString(): String = "_"
}