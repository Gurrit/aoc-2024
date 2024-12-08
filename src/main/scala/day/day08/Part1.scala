package day.day08

import day.Part
import util.InputUtils

object Part1 extends Part {

  override def solve(input: Seq[String]): Unit = {
    val a = InputUtils.to2DIndexedList(input)
    val g = a.groupBy(_._2).view.mapValues(a => a.map(_._1)).filter(_._1 != '.').toMap

    val s = g.flatMap { points =>
      val combinations = points._2.combinations(2).toSeq
      combinations.map { comb =>
        val p1 = comb.head
        val p2 = comb.last
        val diff = ((p1._1 - p2._1), (p1._2 - p2._2))

        Seq((p1._1 + diff._1, p1._2 + diff._2), (p2._1 - diff._1, p2._2 - diff._2))

      }
    }.flatten.filter { fl => fl._1 < input.size && fl._1 >= 0 && fl._2 < input(0).size && fl._2 >= 0}
    val b = s.toSeq.sorted.distinct
    println(b.size)
    }
}
