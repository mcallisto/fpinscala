package fpinscala

import org.scalatest.FlatSpec
import java.util.concurrent._

class chapter7FlatTests extends FlatSpec {

  val Par = fpinscala.parallelism.Par

  val pool: ExecutorService = Executors.newFixedThreadPool(1000)
    
  "The Par library for parallel computation" can "sum a sequence of ints" in {
    val s = (0 to 10).toVector
    assert(Par.run(pool)(Par.sum(s)).get() === 55)
    assert(Par.run(pool)(Par.sumF(s)).get() === 55)
    assert(Par.sumSerial(s) === 55)
  }
  
  it can "create a sequence from parallel computations" in {
    val l = List(Par.unit(1), Par.unit(2), Par.unit(3))
    assert(Par.run(pool)(Par.sequence(l)).get() === List(1, 2, 3))
  }
  
  it can "create filtered parallel computations" in {
    val l = List(1, 2, 3)
    assert(Par.run(pool)(Par.parFilter(l)(_ > 1)).get() === List(2, 3))
  }
  
  it can "find a max in a sequence of ints" in {
    val s = (0 to 10).toVector
    assert(Par.run(pool)(Par.max(s)).get() === 10)
    assert(Par.run(pool)(Par.maxF(s)).get() === 10)
  }
  
  it can "sum the word counts in a list of strings" in {
    val ls = List("The very first paragraph", "This comes second", "A third", "Fourth")
    assert(Par.run(pool)(Par.totalWordsF(ls)).get() === 10)
  }
  
  it can "choose among two computations" in {
    val n = Par.unit(true)
    assert(Par.run(pool)(Par.choice(n)(Par.unit(1), Par.unit(2))).get() === 1)
    assert(Par.run(pool)(Par.choiceViaChoiceN(n)(Par.unit(1), Par.unit(2))).get() === 1)
  }
  
  it can "choose among n given computations" in {
    val n = Par.unit(2)
    val l = List(Par.unit(1), Par.unit(2), Par.unit(3))
    assert(Par.run(pool)(Par.choiceN(n)(l)).get() === 3)
  }
  
}