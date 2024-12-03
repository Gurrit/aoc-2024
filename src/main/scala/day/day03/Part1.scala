package day.day03

import day.Part

import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex

object Part1 extends Part {

  private val mulRegex: Regex = """mul\((\d+),(\d+)\)""".r


  def solve(input: Seq[String]): Unit = {
    val foundRegex = mulRegex.findAllMatchIn(input.mkString)
    val multiplications = foundRegex.map(_.subgroups.pipe(s => (s.head.toLong, s.last.toLong))).toSeq
    val res = multiplications.map(m => m._1 * m._2)

    println(res.sum)
  }

}
