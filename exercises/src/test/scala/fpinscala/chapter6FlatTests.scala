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

  it can "map a result" in {
    val (i, r) = RNG.map(RNG.unit(10))(_ + 1)(seed)
    assert(i === 11)
  }

  it can "generate a random positive Int not greater than a given value" in {
    val (i, r) = RNG.positiveMax(1000)(seed)
    assert(i.isValidInt && i >= 0 && i <= 1000)
  }

  it can "generate a random Double " in {
    val (i, r) = RNG.double(seed)
    val (i2, r2) = RNG.doubleM(r)
    assert(i !== i2)
  }

  it can "generate a random positive Int" in {
    val (i, r) = RNG.positiveInt(seed)
    assert(i.isValidInt && i >= 0)
  }

  it can "generate a list of random Int" in {
    val (i, r) = RNG.ints(5)(seed)
    assert(i.forall(_.isValidInt) && i.size == 5)
  }

  it can "map2 two result" in {
    val (i, r) = RNG.map2(RNG.unit(10), RNG.unit(11))(_ + _)(seed)
    assert(i === 21)
  }

}