package fpinscala

import org.scalatest.FlatSpec
import java.util.concurrent._

class chapter7FlatTests extends FlatSpec {

  val Par = fpinscala.parallelism.Par

  "This Par library for parallel computation" can "sum a sequence of ints" in {
    val s = IndexedSeq(1, 4, 5, 6, 7, 8, 10, 11, 5)
    val pool: ExecutorService = Executors.newFixedThreadPool(100)
    assert(Par.run(pool)(Par.sum(s)).get() === 57)
  }
  
}