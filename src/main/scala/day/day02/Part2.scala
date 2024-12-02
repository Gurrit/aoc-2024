package day.day02

object Part2 {

  def solve(inputs: Seq[String]): Unit = {
    val res = inputs.map { input =>
      val line = input.split(" ").map(_.toInt)
      val head = line.take(2)
      val remaining = line.drop(2)
      val initFailCount = if (isValidSpan(head.head - head.last)) 0 else 1
      val first: Order = if (head.head > head.last) Dec(head.last, initFailCount) else Inc(head.last, initFailCount)

      remaining.foldLeft(first) { (last: Order, current: Int) =>
        isValid(last, current)
      }
    }
    println(res.count(_.fails < 2))
  }

  def isValid(last: Order, current: Int): Order = {
    if (last.fails > 1) {
      last
    } else {
      val diff = current - last.last
      last match {
        case Inc(l, fails) =>
      }
      if (diff > 0) Inc(current, increaseIfFail(last, diff))
      else Dec(current, increaseIfFail(last, diff))
    }
  }

  private def increaseIfFail(last: Order, diff: Int): Int = {
    if (last.isSameSign(diff) && isValidSpan(diff)) last.fails else last.fails + 1
  }

  def isValidSpan(diff: Int): Boolean = {
    diff.abs >= 1 && diff.abs <= 3
  }

  abstract case class Order(val last: Int, val fails: Int) {
    def isSameSign(diff: Int): Boolean
  }
  case class Inc(override val last: Int, override val fails: Int) extends Order(last, fails) {
    override def isSameSign(diff: Int): Boolean = diff > 0
  }
  case class Dec(override val last: Int, override val fails: Int) extends Order(last, fails) {
    override def isSameSign(diff: Int): Boolean = diff < 0
  }

}