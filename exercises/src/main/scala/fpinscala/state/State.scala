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

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // EXERCISE 5: Use map to generate an Int between 0 and n, inclusive.
  def positiveMax(n: Int): Rand[Int] =
    map(rng ⇒ positiveInt(rng))(i ⇒ (i / (Int.MaxValue.toDouble * n)).toInt)

  // EXERCISE 6: Use map to reimplement RNG.double in a more elegant way.
  def doubleM: Rand[Double] =
    map(rng ⇒ positiveInt(rng))(_ / (Int.MaxValue.toDouble + 1))

  // EXERCISE 1: Write a function to generate a random positive integer.
  // Note: you can use x.abs to take the absolute value of an Int, x. Make sure to handle
  // the corner case Int.MinValue, which doesn't have a positive counterpart.  
  def positiveInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (i, r) if (i == Int.MinValue) ⇒ positiveInt(r)
      case (i, r) ⇒ (i.abs, r)
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

  // EXERCISE 7: Unfortunately, map is not powerful enough to implement
  // intDouble and doubleInt from before. What we need is a new combinator
  // map2, that can combine two RNG actions into one using a binary rather than unary
  // function. Write its implementation and then use it to reimplement the intDouble
  // and doubleInt functions.  
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    rng ⇒ {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDoubleM(rng: RNG): Rand[(Int, Double)] =
    map2(rng ⇒ positiveInt(rng), rng2 ⇒ double(rng2))((_, _))

  def doubleIntM(rng: RNG): Rand[(Double, Int)] =
    map2(rng ⇒ double(rng), rng2 ⇒ positiveInt(rng2))((_, _))

  // EXERCISE 8: If we can combine two RNG transitions, we should be able to combine a whole list of them.
  // Implement sequence, for combining a List of transitions into a single transition.
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
//    fs.foldRight(unit(List[A]()))((f, acc) => rng ⇒ {
//      val (a, rng2) = f(rng)
//      val (aa, rng3) = acc(rng2)
//      (a :: aa, rng3)
//    })

  // Use it to reimplement the ints function you wrote before.
  // You can use the standard library function List.fill(n)(x) to make a list with x repeated n times.
  def intsS(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))
    
  // EXERCISE 9: Implement flatMap, then use it to reimplement positiveInt.    
  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] =
    rng ⇒ {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
}

case class State[S, +A](run: S ⇒ (A, S)) {
  def map[B](f: A ⇒ B): State[S, B] =
    sys.error("todo")
  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
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
