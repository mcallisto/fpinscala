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
    val (i2, r2) = RNG.mapF(RNG.unit(10))(_ + 1)(r)
    assert(i2 === 11)
  }

  it can "generate a random positive Int not greater than a given value" in {
    val (i, r) = RNG.positiveMax(1000)(seed)
    assert(i.isValidInt && i >= 0 && i <= 1000)
  }

  it can "generate a random Double" in {
    val (d, r) = RNG.double(seed)
    val (d2, r2) = RNG.doubleM(r)
    assert(d !== d2)
  }

  it can "generate a random positive Int" in {
    val (i, r) = RNG.positiveInt(seed)
    assert(i.isValidInt && i >= 0)
    val (i2, r2) = RNG.positiveIntF(r)
    assert(i2.isValidInt && i2 >= 0)
  }

  it can "generate a list of random Int" in {
    val (l, r) = RNG.ints(5)(seed)
    assert(l.forall(_.isValidInt) && l.size == 5)
    val (l2, r2) = RNG.intsS(5)(r)
    assert(l2.forall(_.isValidInt) && l2.size == 5)
  }

  it can "map2 two results" in {
    val (i, r) = RNG.map2(RNG.unit(10), RNG.unit(11))(_ + _)(seed)
    assert(i === 21)
    val (i2, r2) = RNG.map2F(RNG.unit(10), RNG.unit(11))(_ + _)(r)
    assert(i2 === 21)
  }

  it can "sequence a list of results" in {
    val (i, r) = RNG.sequence(List(RNG.unit(10), RNG.unit(11), RNG.unit(12)))(seed)
    assert(i === List(10, 11, 12))
  }

  it can "flatmap a result" in {
    val (i, r) = RNG.flatMap(RNG.unit(10))(x ⇒ RNG.unit(x + 1))(seed)
    assert(i === 11)
  }
  
}