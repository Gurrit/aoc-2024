package day.day04

import day.Part

object Part1 extends Part {

  private val xmasRegex = """XMAS""".r
  private val samxRegex = """SAMX""".r

  override def solve(input: Seq[String]): Unit = {
    val horizontal = input
    val vertical = input.indices.map { i =>
      input.map(_(i)).mkString
    }
    val maxLength = vertical.length
    val maxDepth = horizontal.length

    val rightDown = horizontal.indices.map { b =>
      vertical.indices.map { a =>
        input((a + b) % maxLength)(a)
      }.mkString.splitAt(maxDepth - b)
    }.flatMap(a => Seq(a._1, a._2))

    val reverseHorizontal = horizontal.reverse

    val leftDown = horizontal.indices.map { b =>
      vertical.indices.map { a =>
        reverseHorizontal(a)((a + b) % maxDepth)
      }.mkString.splitAt(maxLength - b)
    }.flatMap(a => Seq(a._1, a._2))

    val a = findAll(horizontal) ++ findAll(vertical) ++ findAll(rightDown) ++ findAll(leftDown)
    println(a.length)

  }

  private def findAll(a: Seq[String]): Seq[String] =
    a.flatMap { h =>
      xmasRegex.findAllIn(h) ++ samxRegex.findAllIn(h)
    }

}
