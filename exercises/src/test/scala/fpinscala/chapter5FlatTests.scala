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
  
}