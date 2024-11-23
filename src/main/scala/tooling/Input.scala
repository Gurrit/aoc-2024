package tooling

import sttp.client4.quick.*

import java.nio.file.{Files, Paths}
import scala.io.{BufferedSource, Source}

object Input {
  def downloadInputFile(day: Int, token: String): Seq[String] = {
    val year = 2023
    val res = quickRequest
      .header("Cookie", s"session=$token")
      .get(uri"https://adventofcode.com/$year/day/$day/input")
      .send()

    if (!res.is200) {
      println(res)
      throw new Exception(s"Failed to download input for day $day")
    } else {

      // Ugly but works
      val dayResourcePath = Paths.get("src", "main", "resources", day.asFileName)
      Files.createFile(dayResourcePath)
      Files.writeString(dayResourcePath, res.body)
      res.body.linesIterator.toSeq
    }
  }

  def getInput(day: Int): Option[Seq[String]] = getInput(day.asFileName).map(_.getLines().toSeq)
  def getToken(): Option[String] = getInput("token").map(_.mkString)

  def getInput(day: String): Option[BufferedSource] =
    try Some(Source.fromResource(day))
    catch case e: Exception => None

  extension (day: Int) def asFileName = s"day$day.txt"
}