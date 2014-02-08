import zwrss.pl.multialignment.model._

val s1 = Sequence(Seq(Asp, Empty, Leu, Thr))

val s2 = Sequence(Seq(Asp, Empty, Leu, Thr))


val a = Alignment.random(Seq(s1, s2)).stripEmpties

a.score





