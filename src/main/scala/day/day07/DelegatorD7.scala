package day.day07

object DelegatorD7 {

  def delegate(part: Int, input: Seq[String]): Unit =
    part match {
      case 1 => Part1.solve(input)
      case 2 => Part2.solve(input)
    }

}
