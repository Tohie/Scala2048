package twothousand

import scalaz._
import Scalaz._

class Board(private[twothousand] val grid: List[List[Option[Int]]]) {
  def get(x: Int, y: Int) = grid(x)(y)

  def rows = grid.length
  def cols = grid.head.length

  def outOfBounds(x: Int, y: Int) = 
    (x < 0) || (x >= grid.length) || (y < 0) || (y >= grid.head.length)

  def neighbours(x: Int, y: Int): List[Option[Int]] = {
    val coords = List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))

    coords.filterNot {
      case (x, y) => outOfBounds(x, y)
    } map {
      case (x, y) => grid(x)(y)
    }
  }

  def hasSpace = grid.exists( row => row.exists(!_.isDefined) )

  private def canMoveAt(x: Int, y: Int) = neighbours(x, y).sequence match {
    case Some(xs) => xs.exists { _ == get(y, x).getOrElse(0) }
    case None => true 
  }

  def canMove: Boolean = {
    if (hasSpace) return true

    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        if (canMoveAt(j, i)) return true
      }
    }
    return false
  }

  // Shift cells left squashing them together
  // e.g. List(Some(4), None, Some(4)) -> List(Some(8), None, None)
  private def shift(row: List[Option[Int]]): List[Option[Int]] = {
    def squash(elems: List[Option[Int]]) = elems match {
      case (x :: y :: xs) if (x == y) => (x.map(_ * 2) +: xs)
      case _                          => elems
    }
    val grouped = groupWhen(row.filter(_.isDefined)) { _ == _ }
    val shifted = grouped.flatMap(squash(_))
    shifted.padTo(row.length, None)
  }

  def move(direction: String) = new Board(direction match {
    case "left" => grid.map(shift(_))
    case "up" => grid.transpose.map(shift(_)).transpose
    case "right" => grid.map { row => shift(row.reverse).reverse }
    case "down" => grid.transpose.map { row =>
      shift(row.reverse).reverse 
    }.transpose
  })

  def placeRandomPiece: Board = {
    if (!hasSpace) return this

    val x = util.Random.nextInt(rows)
    val y = util.Random.nextInt(cols)
    val value = Some(if (util.Random.nextBoolean) 2 else 4)
    get(x, y) match {
      case Some(_) => placeRandomPiece
      case None    => 
        new Board(grid.updated(x, grid(x).updated(y, value)))
    }
  }

  override def toString() = {
    def pad(s: String): String = s.padTo(5, " ").mkString ++ "|"
    val rows = grid.map { (row: List[Option[Int]]) =>
      row.flatMap { 
        case Some(x) => pad(s"$x")
        case None    => pad(" ")
      }.mkString
    }
    val cols = rows.head.length
    intersperse(rows, List.fill(cols)("-").mkString).mkString("\n")
  }
}
