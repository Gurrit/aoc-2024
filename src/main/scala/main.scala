import tooling.Input

@main
def main(dayInt: String): Unit = {
  setupPreReqs(dayInt.toInt)

}

def runDay(day: Int): Unit = day match {
  case 1 => println("Not implemented")
  case n: Int => println(s"Not Implemented day $n")
}

def setupPreReqs(dayInt: Int): Seq[String] = {
  val token = Input.getToken().getOrElse(throw new Exception("Failed to get token"))
  val maybeInput = Input.getInput(dayInt)
  maybeInput.getOrElse(Input.downloadInputFile(dayInt, token))
}