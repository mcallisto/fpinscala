package fpinscala

import org.scalatest.FlatSpec

class chapter3FlatTests extends FlatSpec {

  val List = fpinscala.datastructures.List
  val Tree = fpinscala.datastructures.Tree
  val Leaf = fpinscala.datastructures.Leaf
  val Branch = fpinscala.datastructures.Branch

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

  "A tree of [Int]" should "have a maximum" in {
    assert(Tree.maximum(Branch(Leaf(1), Branch(Branch(Leaf(5), Leaf(3)), Leaf(2)))) === 5)
  }
  
  "A tree" should "have a size equal to the number of leaves" in {
    assert(Tree.size(Branch(Leaf(1), Branch(Leaf(0), Leaf(2)))) === 3)
    assert(Tree.size(Leaf(2)) === 1)
  }

  it should "have a depth" in {
    assert(Tree.depth(Branch(Leaf(1), Branch(Branch(Leaf(5), Branch(Leaf(8), Leaf(3))), Leaf(2)))) == 5)
    assert(Tree.depth(Leaf(3)) == 1)
    assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(3), Leaf(2)))) == 3)
  } 
  
  it can "be map-ped" in {
    assert(Tree.maximum(Tree.map(Branch(Leaf(1), Branch(Branch(Leaf(5), Branch(Leaf(8), Leaf(3))), Leaf(2))))(_ + 1)) == 9)
  } 
  
  it can "be fold-ed" in {
    assert(Tree.fold(Branch(Leaf(1), Branch(Branch(Leaf(5), Branch(Leaf(8), Leaf(3))), Leaf(2))), 0)(_ + _) == 19)
    assert(Tree.fold(Branch(Leaf("a"), Branch(Branch(Leaf("b"), Branch(Leaf("c"), Leaf("d"))), Leaf("e"))), "-")(_ + _) == "abcde-")
  } 
  
  "Size, maximum, depth and map tree functions" can "be defined in terms of fold" in {
    assert(Tree.sizeF(Branch(Leaf(1), Branch(Leaf(0), Leaf(2)))) === 3)
    assert(Tree.maximumF(Branch(Leaf(1), Branch(Branch(Leaf(5), Leaf(3)), Leaf(2)))) === 5)
    assert(Tree.depthF(Branch(Leaf(1), Branch(Branch(Leaf(5), Branch(Leaf(8), Leaf(3))), Leaf(2)))) == 5)
  }
  
}