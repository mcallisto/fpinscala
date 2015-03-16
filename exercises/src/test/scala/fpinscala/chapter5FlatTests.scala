package fpinscala

import org.scalatest.FlatSpec

class chapter5FlatTests extends FlatSpec {

  val Stream = fpinscala.laziness.Stream

  val x = Stream(1, 2, 3, 4)
  
  "A stream" can "be transformed into a List" in {
    assert(x.toList === List(1, 2, 3, 4))
  }
  
  it can "be reduced with take" in {
    assert(x.take(2).toList === List(1, 2))
    assert(x.takeU(2).toList === List(1, 2))
  }
  
  it can "be reduced with a conditional take" in {
    assert(x.takeWhile(_ % 2 != 0).toList === List(1))
    assert(x.takeWhileR(_ % 2 != 0).toList === List(1))
    assert(x.takeWhileU(_ % 2 != 0).toList === List(1))
  }
  
  it can "be checked with forall" in {
    assert(x.forAll(_ % 2 == 0) === false)
    assert(x.forAll(_ < 5) === true)
  }
  
  it can "be mapped" in {
    assert(x.map(_ + 1).toList === List(2, 3, 4, 5))
    assert(x.mapU(_ + 1).toList === List(2, 3, 4, 5))
  }
  
  it can "be filtered" in {
    assert(x.filter(_ % 2 == 0).toList === List(2, 4))
  }
  
  it can "have another stream appended" in {
    assert(x.append(x).toList === List(1, 2, 3, 4, 1, 2, 3, 4))
  }
  
  it can "be flat mapped" in {
    assert(x.flatMap(x => Stream(x + 1)).toList === List(2, 3, 4, 5))
  }
  
  it can "be a costant streamed infinitely" in {
    assert(Stream.constant(5).take(4).toList === List(5, 5, 5, 5))
    assert(Stream.constantU(5).take(4).toList === List(5, 5, 5, 5))
  }
  
  it can "be a stream of integers infinitely growing" in {
    assert(Stream.from(1).take(4).toList === List(1, 2, 3, 4))
    assert(Stream.fromU(1).take(4).toList === List(1, 2, 3, 4))
  }
  
  it can "be an infinite stream of Fibonacci numbers" in {
    assert(Stream.fibs.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
    assert(Stream.fibsU.take(7).toList === List(0, 1, 1, 2, 3, 5, 8))
  }
  
  it can "be zipped" in {
    assert(x.zip(Stream(4, 6, 8)).toList === List((1, 4), (2, 6), (3, 8)))
  }
  
  it can "be zippedAll" in {
    assert(x.zipAll(Stream(4, 6, 8)).toList === List((Some(1), Some(4)), (Some(2), Some(6)), (Some(3), Some(8)), (Some(4), None)))
  }
  
  it can "be checked if starts with another stream" in {
    assert(x.startsWith(Stream(1, 2, 3)))
    assert(x.startsWith(x))
    assert(x.startsWith(Stream(1, 2, 3, 4, 5)) === false)
    assert(x.startsWith(Stream(1, 2, 5)) === false)
  }
  
  it can "be reduced with drop" in {
    assert(x.drop(2).toList === List(3, 4))
  }  
  
  it can "be converted into stream of tails" in {
    assert(x.tails.map(_.toList).toList ===
      List(List(1, 2, 3, 4), List(2, 3, 4), List(3, 4), List(4), List()))
  }  
  
  it can "be checked if contains a subsequence" in {
    assert(x.hasSubsequence(Stream(2, 3)))
    assert(x.hasSubsequence(Stream(2, 4)) == false)
  }  
  
}