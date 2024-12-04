package day.day04

import day.Part

private val xmas = "MAS"
private val samx = "SAM"

object Part2 extends Part {

  override def solve(input: Seq[String]): Unit = {
    val matrix = input.map(_.toSeq)
    val xmases = (1 until (matrix.length - 1)).flatMap { i =>
      (1 until (matrix.length - 1)).map { j =>
        Seq(matrix(i-1)(j-1), matrix(i)(j), matrix(i+1)(j+1)) ++
          Seq(matrix(i+1)(j-1), matrix(i)(j), matrix(i-1)(j+1))
      }
    }.map(_.mkString)

    val possibilities = Seq(xmas + xmas, xmas + samx, samx + xmas, samx + samx)

    val sol = xmases.filter(possibilities.contains(_))
    println(sol.length)

  }

}
