package day.day08

import day.Part

object Part2 extends Part {


  override def solve(input: Seq[String]): Unit = {
    val a = input.zipWithIndex.flatMap(a => a._1.zipWithIndex.map(b => ((a._2, b._2), b._1)))

    val g = a.groupBy(_._2).view.mapValues(a => a.map(_._1)).filter(_._1 != '.').toMap

    val s = g.flatMap { points =>
      val combinations = points._2.combinations(2).toSeq
      combinations.map { comb =>
        val p1 = comb.head
        val p2 = comb.last
        val diff = ((p1._1 - p2._1), (p1._2 - p2._2))

        (-50 until 50).flatMap { mul =>
          Seq((p1._1 + (diff._1 * mul), p1._2 + diff._2 * mul), (p2._1 - diff._1 * mul, p2._2 - diff._2 * mul))
        }
      }
    }.flatten.filter { fl => fl._1 < input.size && fl._1 >= 0 && fl._2 < input(0).size && fl._2 >= 0}
    val b = s.toSeq.sorted.distinct

    println(b)
    println(b.size)
  }
}
