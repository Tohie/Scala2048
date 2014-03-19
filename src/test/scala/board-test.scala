package twothousand

import org.specs2.mutable._

class BoardSpec extends Specification {
  val stuck = new Board(List(List(Some(4), Some(8))))
  "The stuck board" should {
    "not be able to move" in {
      stuck.canMove must beFalse
    }
    "not have space" in {
      stuck.hasSpace must beFalse
    }
  }

  val movable = new Board(List(
    List(Some(4), None, Some(4), None),
    List(Some(4), None, None, None)
  ))
  "Movable" should {
    "be able to move" in {
      movable.canMove must beTrue
    }
    "move correctly" in {
      val movedLeft = new Board(List(
        List(Some(8), None, None, None),
        List(Some(4), None, None, None)
      ))
      movable.move("left").grid mustEqual movedLeft.grid

      val movedUp = new Board(List(
        List(Some(8), None, Some(4), None),
        List(None, None, None, None)
      ))
      movable.move("up").grid mustEqual movedUp.grid
    }
    "have the correct neighbours" in {
      movable.neighbours(0, 0) mustEqual List(Some(4), None)
      movable.neighbours(0, 2) mustEqual List.fill(3)(None)
    }
    "place a random piece" in {
      movable.placeRandomPiece.grid mustNotEqual movable.grid
    }
    "work out whats within it's bounds" in {
      movable.outOfBounds(0, 0) must beFalse
      movable.outOfBounds(10, 0) must beTrue
      movable.outOfBounds(0, 10) must beTrue
    }
  }
}
