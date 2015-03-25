package fpinscala

import org.scalatest.FlatSpec
import java.util.concurrent._

class chapter7FlatTests extends FlatSpec {

  val Par = fpinscala.parallelism.Par

  val pool: ExecutorService = Executors.newFixedThreadPool(1000)
    
  "The Par library for parallel computation" can "sum a sequence of ints" in {
    val s = (0 to 10).toVector
    assert(Par.run(pool)(Par.sum(s)).get() === 55)
    assert(Par.sumSerial(s) === 55)
  }
  
  it can "create a sequence from parallel computations" in {
    val l = List(Par.unit(1), Par.unit(2), Par.unit(3))
    assert(Par.run(pool)(Par.sequence(l)).get() == List(1, 2, 3))
  }
  
  it can "create filtered parallel computations" in {
    val l = List(1, 2, 3)
    assert(Par.run(pool)(Par.parFilter(l)(_ > 1)).get() == List(2, 3))
  }
  
  it can "find a max in a sequence of ints" in {
    val s = (0 to 10).toVector
    assert(Par.run(pool)(Par.max(s)).get() === 10)
  }
  
}