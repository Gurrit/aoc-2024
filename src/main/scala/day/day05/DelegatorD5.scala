package day.day05

object DelegatorD5 {

  def delegate(part: Int, input: Seq[String]): Unit =
    part match {
      case 1 => Part1.solve(input)
      case 2 => Part2.solve(input)
    }
}
