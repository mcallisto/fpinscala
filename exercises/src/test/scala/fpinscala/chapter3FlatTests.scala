package fpinscala

import org.scalatest.FlatSpec

class chapter3FlatTests extends FlatSpec {

  val List = fpinscala.datastructures.List

  "A list" can "be concatenated" in {
    assert(List.concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8))) === List(1, 2, 3, 4, 5, 6, 7, 8))
  }
  
  "A list" can "be folded left to sum all its elements" in {
    assert(List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) == 10)
  }  
}