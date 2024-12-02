package day.day01

object DelegatorD1 {

  def delegator(part: Int, input: Seq[String]): Unit =
    part match {
      case 1 => Part1.solve(input)
      case 2 => Part2.solve(input)
    }
}
