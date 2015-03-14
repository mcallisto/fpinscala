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

  def takeWhile(p: A ⇒ Boolean): Stream[A] = sys.error("todo")

  def forAll(p: A ⇒ Boolean): Boolean = sys.error("todo")

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
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] = sys.error("todo")
}