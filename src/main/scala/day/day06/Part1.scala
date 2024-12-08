package day.day06

import day.Part

object Part1 extends Part {

  override def solve(input: Seq[String]): Unit = {
    val matrix = input.map { a => a.toCharArray.toSeq}
    val maxY = matrix.length
    val maxX = matrix.head.length

    var facing = getFacing(matrix)
    val picked: scala.collection.mutable.Set[(Int, Int)] = scala.collection.mutable.Set()

    while(inBounds(facing, maxX, maxX)) {
      picked.add(facing.x, facing.y)
      if (!peak(facing, matrix)) {
        facing = facing match
          case Up(x, y) => Right(x, y).moveOne()
          case Right(x, y) => Down(x, y).moveOne()
          case Down(x, y) => Left(x, y).moveOne()
          case Left(x, y) => Up(x, y).moveOne()
      } else {
        facing = facing.moveOne()
      }
    }
    picked.add(facing.x, facing.y)
    println(picked.size)
  }

  private def inBounds(facing: Facing, maxX: Int, maxY: Int): Boolean = {
    facing.x < maxX-1 && facing.x >= 1 &&
      facing.y < maxY-1 && facing.y >= 1
  }


  private def peak(facing: Facing, matrix: Seq[Seq[Char]]): Boolean = {
    val next = facing.moveOne()
    matrix(next.y)(next.x) != '#'
  }

  private def getFacing(table: Seq[Seq[Char]]): Facing = {
    val y = table.indexWhere(
      _.contains('^')
    )
    val x = table(y).indexOf('^')
    Up(x, y)
  }

  private abstract class Facing(val x: Int, val y: Int) {
    def moveOne(): Facing

  }

  private case class Up(override val x: Int, override val y: Int) extends Facing(x, y) {
    override def moveOne(): Facing = Up(x, y - 1)
  }
  private case class Down(override val x: Int, override val y: Int) extends Facing(x, y) {
    override def moveOne(): Facing = Down(x, y + 1)
  }
  private case class Left(override val x: Int, override val y: Int) extends Facing(x, y) {
    override def moveOne(): Facing = Left(x - 1, y)
  }
    private case class Right(override val x: Int, override val y: Int) extends Facing(x, y) {
      override def moveOne(): Facing = Right(x + 1, y)
    }
}
