package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: ⇒ B)(f: (A, ⇒ B) ⇒ B): B = // The arrow `⇒` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) ⇒ f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _         ⇒ z
    }

  def exists(p: A ⇒ Boolean): Boolean = 
    foldRight(false)((a, b) ⇒ p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A ⇒ Boolean): Option[A] = this match {
    case Empty      ⇒ None
    case Cons(h, t) ⇒ if (f(h())) Some(h()) else t().find(f)
  }
  
  // EXERCISE 1: Write a function to convert a Stream to a List,
  // which will force its evaluation and let us look at it in the REPL.
  // You can convert to the regular List type in the standard library.
  // You can place this and other functions that accept a Stream inside the Stream trait.
  def toList(): List[A] = {
    def go(a: Stream[A], acc: List[A]): List[A] = a match {
      case Empty      ⇒ acc
      case Cons(h, t) ⇒ go(t(), acc :+ h())
    }
    
    go(this, Nil)
  }
//    this match { 
//      case Empty      ⇒ Nil
//      case Cons(h, t) ⇒ h() :: t().toList
//    }   

  //EXERCISE 2: Write a function take for returning the first n elements of a Stream.
  def take(n: Int): Stream[A] = this match {
    case Empty        ⇒ empty
    case _ if (n < 1) ⇒ empty
    case Cons(h, t)   ⇒ cons(h(), t() take(n - 1))
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  // EXERCISE 3: Write the function takeWhile for returning all starting
  // elements of a Stream that match the given predicate.  
  def takeWhile(p: A ⇒ Boolean): Stream[A] = this match {
    case Empty                  ⇒ empty
    case Cons(h, t) if (p(h())) ⇒ cons(h(), t() takeWhile(p))
    case _                      ⇒ empty
  }
  
  // EXERCISE 5: Use foldRight to implement takeWhile. This will construct a stream incrementally,
  // and only if the values in the result are demanded by some other expression.  
  def takeWhileR(p: A ⇒ Boolean): Stream[A] =
    foldRight(Stream(): Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)
  
  // EXERCISE 4: Implement forAll, which checks that all elements in the
  // Stream match a given predicate. Your implementation should terminate the
  // traversal as soon as it encounters a non-matching value.
  def forAll(p: A ⇒ Boolean): Boolean =
    foldRight(true)((a, b) ⇒ p(a) && b)

  // EXERCISE 6: Implement map, filter, append, and flatMap using foldRight.
  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ cons(f(a), b))
    
  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, b) ⇒ if (p(a)) cons(a, b) else b)    
    
  def append[B >: A](b: Stream[B]): Stream[B] =
    foldRight(b)(cons(_, _))
    
  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) ⇒ f(a) append b)
    
  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () ⇒ A, t: () ⇒ Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() ⇒ head, () ⇒ tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  // EXERCISE 7: Generalize ones slightly to the function constant which
  // returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))
  
  // EXERCISE 8: Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, etc.
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  
  // EXERCISE 9: Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  val fibs: Stream[Int] = {  
     def go(beforelast: Int, last: Int): Stream[Int] =
       cons(beforelast + last, go(last, beforelast + last))

     Stream(0, 1).append(go(0, 1))
  }  
  
  // EXERCISE 10: We can write a more general stream building function.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream. 
  // Option is used to indicate when the Stream should be terminated, if at all.
  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = f(z) match {
    case None         ⇒ empty
    case Some((a, s)) ⇒ cons(a, unfold(s)(f))
  }
  
  // EXERCISE 11: Write fibs, from, constant, and ones in terms of unfold.
  val fibsU: Stream[Int] = 
    Stream(0, 1) append unfold((0, 1))(_ match {
      case (beforelast, last) ⇒ Some(beforelast + last, (last, beforelast + last)) 
    })
  
  def fromU(n: Int): Stream[Int] = unfold(n)(x ⇒ Some(x, x + 1))
  
  def constantU(n: Int): Stream[Int] = unfold(n)(x ⇒ Some(x, x))

  val onesU: Stream[Int] = unfold(1)(x ⇒ Some(x, x))
  
}