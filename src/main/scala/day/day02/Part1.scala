package day.day02

object Part1 {

  def solve(inputs: Seq[String]): Unit = {
    val res = inputs.map { input =>
      val line = input.split(" ").map(_.toInt)
      val head = line.take(2)
      val remaining = line.drop(2)

      val first: Order = if (head.head > head.last) Dec(head.last, isValidSpan(head.head - head.last)) else Inc(head.last, isValidSpan(head.head - head.last))

      remaining.foldLeft(first) { (last: Order, current: Int) =>
        isValid(last, current)
      }
    }
    println(res.count(_.valid))
  }

  def isValid(last: Order, current: Int): Order = {
    if (!last.valid)
      last
    else {
      val diff = current - last.last
      if (diff > 0) Inc(current, last.isSameSign(diff) && isValidSpan(diff))
      else Dec(current, last.isSameSign(diff) && isValidSpan(diff))
    }
  }

  def isValidSpan(diff: Int): Boolean = {
    diff.abs >= 1 && diff.abs <= 3
  }

  abstract class Order(val last: Int, val valid: Boolean) {
    def isSameSign(diff: Int): Boolean
  }
  case class Inc(override val last: Int, override val valid: Boolean) extends Order(last, valid) {
    override def isSameSign(diff: Int): Boolean = diff > 0
  }
  case class Dec(override val last: Int, override val valid: Boolean) extends Order(last, valid) {
    override def isSameSign(diff: Int): Boolean = diff < 0
  }

}