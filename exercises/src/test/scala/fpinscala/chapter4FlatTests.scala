package fpinscala

import org.scalatest.FlatSpec

class chapter4FlatTests extends FlatSpec {

  val Option = fpinscala.errorhandling.Option

  "A sequence of doubles" can "have a mean" in {
    assert(Option.mean(Seq(1.0, 2.0, 6.0)).getOrElse(123.0) === 3.0)
  }
  
  it should "not have a mean if the sequence is empty" in {
    assert(Option.mean(Seq()).getOrElse(123.0) === 123.0)
  }
  
  it can "have a variance" in {
    assert(Option.variance(Seq(1.0, 2.0)).getOrElse(123.0) === 0.25)
  }
  
  it should "not have a variance if the sequence is empty" in {
    assert(Option.variance(Seq()).getOrElse(123.0) === 123.0)
  }
  
}