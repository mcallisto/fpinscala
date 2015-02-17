package fpinscala

import org.scalatest.FlatSpec

class chapter3FlatTests extends FlatSpec {

  val List = fpinscala.datastructures.List

  "A list" can "be concatenated" in {
    assert(List.concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8))) === List(1, 2, 3, 4, 5, 6, 7, 8))
  }
  
  "A list" can "be folded left" in {
    assert(List.foldLeft(List("a", "b", "c"), "-")(_ + _) == "-abc")
  }
  
  it can "be folded right" in {
    assert(List.foldRight(List("a", "b", "c"), "-")(_ + _) == "abc-")
  }
  
  it can "be folded left with a foldRight based version" in {
    assert(List.foldLeft2(List("a", "b", "c"), "-")(_ + _) == "-abc")
  }
  
  it can "be folded right with a foldLeft based version" in {
    assert(List.foldRight2(List("a", "b", "c"), "-")(_ + _) == "abc-")
  }
  
  val l = List(1, 2, 3, 4)
  "List 1,2,3,4" should "consider 2 as a subsequence" in {
    assert(List.hasSubsequence(l, List(2)) === true)
  } 

  it should "contain 3" in {
    assert(List.contain(l, 3) === true) 
  }
  
  it should "NOT contain 5" in {
    assert(List.contain(l, 5) === false) 
  }
  
  it should "have 1,2 as first two elements" in {
    assert(List.take(l, 2) === List(1, 2)) 
  }
  
  it should "have 2,3 as elements from 1 until 3" in {
    assert(List.slice(l, 1, 3) === List(2, 3)) 
  }
  
  it can "be put through a slider" in {
    assert(List.slide(l, 2) === List(List(1, 2), List(2, 3), List(3, 4))) 
  }
  
  it should "consider 2,3 as a subsequence" in {
    assert(List.hasSubsequence(l, List(2, 3)) === true)
  } 

  it should "NOT consider 3,2 as a subsequence" in {
    assert(List.hasSubsequence(l, List(3, 2)) === false)
  } 

}