package day.day01

import day.Part
import util.InputUtils

object Part2 extends Part {
  def solve(inputs: Seq[String]) = {
    val lines = InputUtils.linesInt(inputs)

    val left = lines._1.sorted
    val right = lines._2.sorted

    val count = right.groupBy(a => a).map(a => (a._1, a._2.size))

    val values = left.map {a => a * count.getOrElse(a, 0) }

    val value = values.sum
    println(value)

  }
}