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

  def positiveIntF: Rand[Int] =
    flatMap(int)(i ⇒ if (i != Int.MinValue) unit(i.abs) else positiveIntF)

  // EXERCISE 10: Reimplement map and map2 in terms of flatMap
  def mapF[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    flatMap(s)(a ⇒ unit(f(a)))

  def map2F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
    flatMap(ra)(a ⇒ flatMap(rb)(b ⇒ unit(f(a, b))))
}

import State._

// EXERCISE 11: Generalize the functions unit, map, map2, flatMap, and sequence.
// Add them as methods on the State case class where possible.
// Otherwise you should put them in a State companion object.
case class State[S, +A](run: S ⇒ (A, S)) {

  def map[B](f: A ⇒ B): State[S, B] =
    flatMap(a ⇒ unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    sb.flatMap(b ⇒ flatMap(a => unit(f(a, b))))
  //    State(s ⇒ {
  //      val (b, s1) = sb.run(s)
  //      val (a, s2) = run(s1)
  //      (f(a, b), s2)
  //    })

  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    State(s ⇒ {
      val (a, s1) = run(s)
      val (b, s2) = f(a).run(s1)
      (b, s2)
    })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s ⇒ (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) ⇒ f.map2(acc)(_ :: _))

  def sequenceLeft[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List[A]()))((acc, f) ⇒ f.map2(acc)(_ :: _))

  def modify[S](f: S ⇒ S): State[S, Unit] = for {
    s ← get
    _ ← set(f(s))
  } yield ()

  // EXERCISE 12: Come up with the signatures for get and set, then write their implementations.
  // A combinator get for getting the current state, and a combinator set for setting a new state
  def get[S]: State[S, S] =
    State(s ⇒ (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ ⇒ ((), s))

}

// EXERCISE 13: To gain experience with the use of State,
// implement a simulation of a simple candy dispenser.
// The machine has two types of input: You can insert a coin, or you can turn the knob to dispense candy.
// It can be in one of two states: locked or unlocked.
// It also tracks how many candies are left and how many coins it contains.

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

// The rules of the machine are as follows:
// Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
// Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
// Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
// A machine that is out of candy ignores all inputs.
object Candy {

  // The method simulateMachine should operate the machine based on the list of inputs
  // and return the number of coins left in the machine at the end.
  // Note that if the input Machine has 10 coins in it,
  // and a net total of 4 coins are added in the inputs, the output will be 14.
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ ← sequence(inputs.map(i ⇒ modify((s: Machine) ⇒ (i, s) match {
        case (_,    Machine(_,     0,     _)) ⇒ s
        case (Coin, Machine(false, _,     _)) ⇒ s
        case (Turn, Machine(true,  _,     _)) ⇒ s
        case (Coin, Machine(true,  candy, coin)) ⇒ Machine(false, candy,     coin + 1)
        case (Turn, Machine(false, candy, coin)) ⇒ Machine(true,  candy - 1, coin)
      })))
      s ← get
    } yield (s.coins, s.candies)
}
