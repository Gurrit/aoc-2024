import day.day01.DelegatorD1
import day.day02.DelegatorD2
import day.day03.DelegatorD3
import day.day04.DelegatorD4
import day.day05.DelegatorD5
import day.day06.DelegatorD6
import day.day07.DelegatorD7
import day.day08.DelegatorD8
import tooling.Input

@main
def main(dayInt: String, partInt: String): Unit = {
  val day = dayInt.toInt
  val part = partInt.toInt
  val input = setupPreReqs(day)
  runDay(day, part, input)
}

def runDay(day: Int, part: Int, input: Seq[String]): Unit = day match {
  case 1 => DelegatorD1.delegate(part, input)
  case 2 => DelegatorD2.delegate(part, input)
  case 3 => DelegatorD3.delegate(part, input)
  case 4 => DelegatorD4.delegate(part, input)
  case 5 => DelegatorD5.delegate(part, input)
  case 6 => DelegatorD6.delegate(part, input)
  case 7 => DelegatorD7.delegate(part, input)
  case 8 => DelegatorD8.delegate(part, input)
  case n: Int => println(s"Not Implemented day $n")
}

def setupPreReqs(dayInt: Int): Seq[String] = {
  val token = Input.getToken().getOrElse(throw new Exception("Failed to get token"))
  val maybeInput = Input.getInput(dayInt)
  maybeInput.getOrElse(Input.downloadInputFile(dayInt, token))
}