package day.day01

import util.InputUtils

object Part1 {

  def solve(inputs: Seq[String]): Unit = {
    val lines = InputUtils.linesInt(inputs)

    val left = lines._1.sorted
    val right = lines._2.sorted

    val diff = left.zip(right).map { l =>
      (l._1 - l._2).abs
    }

    println(s"$left  $right")

    println(diff.sum)
  }
}
