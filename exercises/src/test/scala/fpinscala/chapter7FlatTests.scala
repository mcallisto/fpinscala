package fpinscala

import org.scalatest.FlatSpec
import java.util.concurrent._

class chapter7FlatTests extends FlatSpec {

  val Par = fpinscala.parallelism.Par

  "The Par library for parallel computation" can "sum a sequence of ints" in {
    val s = (0 to 10).toVector
    val pool: ExecutorService = Executors.newFixedThreadPool(1000)
    assert(Par.run(pool)(Par.sum(s)).get() === 55)
    assert(Par.sumSerial(s) === 55)
  }
  
}