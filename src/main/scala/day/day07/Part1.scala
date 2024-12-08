package day.day07

import day.Part

object Part1 extends Part {

  override def solve(input: Seq[String]): Unit = {
    val line: Seq[(Long, Seq[Long])] = input.map { l =>
      val ln = l.split(":")
      (ln.head.toLong, ln.last.trim.split(" ").map(_.toLong))
    }


    val p = line.filter { (result, operands) =>
      val possible = createOpTree(operands.reverse)
      possible.contains(result)
    }

    println(p.map(_._1).sum)
  }

  def createOpTree(operands: Seq[Long]): Seq[Long] = {
    if (operands.size == 1) {
      Seq(operands.head)
    } else {
      val mul = createOpTree(operands.tail).map(_ * operands.head)
      val add = createOpTree(operands.tail).map(_ + operands.head)
      mul ++ add
    }

  }

  abstract class Operator
  case class Tree(op1: Operator, op2: Operator) extends Operator
  case class Mul(op1: Operator, op2: Operator) extends Operator
  case class Add(op1: Operator, op2: Operator) extends Operator
  case class Operand(value: Long) extends Operator
}
