import day.day01.DelegatorD1
import day.day02.DelegatorD2
import day.day03.DelegatorD3
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
  case n: Int => println(s"Not Implemented day $n")
}

def setupPreReqs(dayInt: Int): Seq[String] = {
  val token = Input.getToken().getOrElse(throw new Exception("Failed to get token"))
  val maybeInput = Input.getInput(dayInt)
  maybeInput.getOrElse(Input.downloadInputFile(dayInt, token))
}