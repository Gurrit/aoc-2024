package day.day05

import day.Part

object Part1 extends Part {

  override def solve(input: Seq[String]): Unit = {
    val splitPoint = input.indexWhere("" == _)
    val (rulesStrings, inp) = input.splitAt(splitPoint)

    val rules = rulesStrings.map { rule =>
      val split = rule.split('|')
      (split.head.toInt, split.last.toInt)
    }.groupBy(_._1)

    val ints = inp.tail.map(_.split(',').map(_.toInt).toSeq)
    val res = ints.filter { intLine =>
      intLine.forall { lineVal =>
          val maybeRulesFor = rules.get(lineVal)
            .map(rs => // Optional
              rs.map(_._2))
        maybeRulesFor.forall { rulesFor =>
          val intersection = rulesFor.intersect(intLine.toSeq)
          intersection.forall(a =>
            intLine.indexOf(lineVal) < intLine.indexOf(a)
          )
        }
    }
    }
    println(res.map { a =>
      a(a.size / 2)
    }.sum)

  }
}
