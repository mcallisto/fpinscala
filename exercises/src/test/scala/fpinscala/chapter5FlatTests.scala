package fpinscala

import org.scalatest.FlatSpec

class chapter5FlatTests extends FlatSpec {

  val Stream = fpinscala.laziness.Stream

  val x = Stream(1, 2, 3, 4)
  
  "A stream" can "be transformed into a List" in {
    assert(x.toList === List(1, 2, 3, 4))
  }
  
  it can "can be reduced with take" in {
    assert(x.take(2).toList === List(1, 2))
  }
  
  it can "can be reduced with a conditional take" in {
    assert(x.takeWhile(_ % 2 == 0).toList === List(2, 4))
    assert(x.takeWhileR(_ % 2 == 0).toList === List(2, 4))
  }
  
  it can "can be checked with forall" in {
    assert(x.forAll(_ % 2 == 0) === false)
    assert(x.forAll(_ < 5) === true)
  }
  
}