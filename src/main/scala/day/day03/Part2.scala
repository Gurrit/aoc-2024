package day.day03

import day.Part

object Part2 extends Part {

    private val mulRegex: String = """mul\((\d+),(\d+)\)"""
    private val doRegex: String = """(do\(\))"""
    private val dontRegex: String = """(don\'t\(\))"""


    def solve(input: Seq[String]): Unit = {
      val fullRegex = s"$mulRegex|$doRegex|$dontRegex".r

      val foundOps = fullRegex.findAllMatchIn(input.mkString).toSeq
      val res = foundOps.map(_.subgroups).foldLeft((0, true)) { (acc, op) =>
        op match {
          case Seq(a, b, null, null) if acc._2 => {
            (acc._1 + a.toInt * b.toInt, acc._2)
          }
          case Seq(null, null, _, null) => (acc._1, true)
          case Seq(null, null, null, _) => (acc._1, false)
          case _ => acc
        }
      }
      println(res._1)

    }
}
