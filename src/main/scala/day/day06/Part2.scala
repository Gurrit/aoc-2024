package day.day06

import day.Part

object Part2 extends Part {

  override def solve(input: Seq[String]): Unit = {
    val matrix = input.map { a => a.toCharArray.toSeq }

    val maxY = input.length
    val maxX = input.head.length

    val originalFacing = getFacing(matrix)

    val picked: scala.collection.mutable.Set[Facing] = scala.collection.mutable.Set()
    val start = getFacing(matrix)
    var facing = start
    while (inBounds(facing, maxX, maxX)) {
      picked.add(facing)
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
    picked += facing

    val c = picked.foldLeft(Seq.empty[Facing]) { (acc, f) =>
      if (!(matrix(f.y)(f.x) == '^')) {
        if (acc.exists(a => a.y == f.y && a.x == f.x)) {
          acc
        } else {
          val newMatrix = matrix.updated(f.y, matrix(f.y).updated(f.x, '0'))
          val a: Option[Facing] = getTouched(originalFacing, newMatrix, maxX, maxY, start, acc, f)
          if (a.isDefined) {
            acc.appended(f)
          } else acc
        }
      }
      else acc
    }
    println(c.size)

  }

  private def getTouched(
                          facing: Facing,
                          matrix: Seq[Seq[Char]],
                          maxX: Int,
                          maxY: Int,
                          start: Facing,
                          alreadyFound: Seq[Facing],
                          updated: Facing
                        ): Option[Facing] = {
    val picked: scala.collection.mutable.Set[Facing] = scala.collection.mutable.Set()
    var facing = getFacing(matrix)
    while(inBounds(facing, maxX, maxX) ) {
      if (
        !(facing.x == start.x && facing.y == start.y) &&
          !picked.add(facing)   &&
          !alreadyFound.exists(a => a.y == updated.y && a.x == updated.x)) {
        return Some(facing)
      }
      if (!peak(facing, matrix)) {
        facing = facing match
          case Up(x, y) => Right(x, y)
          case Right(x, y) => Down(x, y)
          case Down(x, y) => Left(x, y)
          case Left(x, y) => Up(x, y)
      } else {
        facing = facing.moveOne()
      }
    }
    None
  }

  private def inBounds(facing: Facing, maxX: Int, maxY: Int): Boolean = {
    facing.x < maxX-1 && facing.x >= 1 &&
      facing.y < maxY-1 && facing.y >= 1
  }


  private def peak(facing: Facing, matrix: Seq[Seq[Char]]): Boolean = {
    val next = facing.moveOne()
    matrix(next.y)(next.x) != '#' && matrix(next.y)(next.x) != '0'
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