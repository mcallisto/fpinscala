package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
    
  }
   
  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng ⇒ (a, rng)

  def map[A,B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  
  // EXERCISE 1: Write a function to generate a random positive integer.
  // Note: you can use x.abs to take the absolute value of an Int, x. Make sure to handle
  // the corner case Int.MinValue, which doesn't have a positive counterpart.  
  def positiveInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) if (i == Int.MinValue) ⇒ positiveInt(r)
      case (i, r)                        ⇒ (i.abs, r)
    }
    
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  // EXERCISE 2: Write a function to generate a Double between 0 and 1, not including 1.
  // Note: you can use Int.MaxValue to obtain the maximum positive integer value
  // and you can use x.toDouble to convert an Int, x, to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = positiveInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // EXERCISE 3: Write functions to generate an (Int, Double) pair, a
  // (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
  // You should be able to reuse the functions you've already written.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = positiveInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng: RNG)
    ((d, i), r)
  }
    
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

  // EXERCISE 4: Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = { 
    (0 until count).foldLeft((Nil: List[Int], rng))((x, _) => {
      val (i, r) = x._2.nextInt
      (i :: x._1, r)
    })
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S ⇒ (A, S)) {
  def map[B](f: A ⇒ B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
