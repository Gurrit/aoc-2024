package util

object InputUtils {

  def linesInt(inputs: Seq[String], split: String = " "): (Seq[Int], Seq[Int]) = {
    inputs.map { input =>
      input.split(split)
    }.map(s => (s.head.toInt, s.last.toInt))
  }.unzip
  
}
