package util

object InputUtils {

  def linesInt(inputs: Seq[String], split: String = " "): (Seq[Int], Seq[Int]) = {
    inputs.map { input =>
      input.split(split)
    }.map(s => (s.head.toInt, s.last.toInt))
  }.unzip
  
  def to2DIndexedList(input: Seq[String]): Seq[((Int, Int), Char)] = 
    input.zipWithIndex.flatMap(a => a._1.zipWithIndex.map(b => ((a._2, b._2), b._1)))
  
}
