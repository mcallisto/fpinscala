package fpinscala

import org.scalatest.FlatSpec

class chapter6FlatTests extends FlatSpec {

  val RNG = fpinscala.state.RNG

  val seed = RNG.Simple(12234455)
  
  "The Random library" can "generate a random Int" in {
    val (i, r) = RNG.int(seed)
    assert(i.isValidInt)
  }

 it should "generate the same random Int given the same seed (state)" in {
    val (i, r) = RNG.int(seed)
    val (i2, r2) = RNG.int(seed)
    assert(i === i2)
  }

  it should "generate a different random Int given a different seed (state)" in {
    val (i, r) = RNG.int(seed)
    val (i2, r2) = RNG.int(r)
    assert(i !== i2)
  }

  it can "generate a given value" in {
    val (i, r) = RNG.unit(10)(seed)
    assert(i === 10)
  }

}