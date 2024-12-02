package day.day03

object DelegatorD3 {

  def delegate(part: Int, input: Seq[String]): Unit =
    part match {
      case 1 => Part1.solve(input)
      case 2 => Part2.solve(input)
    }
}
