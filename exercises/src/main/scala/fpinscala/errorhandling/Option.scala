package fpinscala.errorhandling


import scala.{Option ⇒ _, Some ⇒ _, Either ⇒ _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // Exercise 1: Implement all of the below functions
  def map[B](f: A ⇒ B): Option[B] = this match {
    case Some(x) ⇒ Some(f(x))
    case None    ⇒ None
  }

  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case Some(x) ⇒ x
    case None    ⇒ default
  }
  
  def flatMap[B](f: A ⇒ Option[B]): Option[B] = map(f) getOrElse None
//  this match {
//    case Some(x) ⇒ f(x)
//    case None    ⇒ None
//  }

  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = map(Some(_)) getOrElse ob 
//  this match {
//    case Some(_) ⇒ this
//    case None    ⇒ ob
//  }

  def filter(f: A ⇒ Boolean): Option[A] = flatMap(x ⇒ if (f(x)) Some(x) else None)
//  this match {
//    case Some(x) if (f(x)) ⇒ this
//    case _                 ⇒ None
//  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception ⇒ 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception ⇒ 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // EXERCISE 2: Implement the variance function
  // (if the mean is m, variance is the mean of math.pow(x - m, 2), see definition)
  // in terms of mean and flatMap
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m ⇒ mean(xs map (x ⇒ math.pow(x - m, 2))))
//    mean(xs) match {
//      case Some(m) ⇒ mean(xs map (x ⇒ math.pow(x - m, 2)))
//      case None    ⇒ None
//    }
  }

  // EXERCISE 3: Write a generic function map2, that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] =
    a flatMap (x ⇒ b map(y ⇒ f(x, y)))
//    a match {
//      case Some(x) ⇒ b map(y ⇒ f(x, y))
//      case None ⇒ None
//    }
  
  // EXERCISE 4: Re-implement bothMatch above in terms of map2, to the extent possible.
  //  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
  //    mkMatcher(pat) flatMap (f => mkMatcher(pat2) map (g => f(s) && g(s)))
  def bothMatch(pat: String, pat2: String, s: String)(f: String ⇒ Option[Boolean]): Option[Boolean] =
    map2(f(pat), f(pat2))(_ && _)

  // EXERCISE 5: Write a function sequence, that combines a list of Options
  // into one option containing a list of all the Some values in the original list.
  // If the original list contains None even once, the result of the function should be None,
  // otherwise the result should be Some with a list of all the values.
  // This is a clear instance where it's not possible to define the function in the OO style.
  // This should not be a method on List (which shouldn't need to know anything about Option),
  // and it can't be a method on Option.
  // Break the list out using pattern-matching
  // where there will be a recursive call to `sequence` in the cons case.
  // Alternatively, use the `foldRight` method to take care of the recursion for you.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((x, y) => map2(x, y)(_ :: _))    
//    def go[A](a: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = a match {
//      case Nil => acc
//      case x :: xs => go(xs, map2(x, acc)((j, k) => j :: k))
//    }
//    
//    go(a, Some(List()): Option[List[A]])
  }

  // EXERCISE 6: Implement the traverse function.
  // It is straightforward to do using map and sequence,
  // but try for a more efficient implementation that only looks at the list once.
  // In fact, implement sequence in terms of traverse.
  def traverse[A, B](a: List[A])(f: A ⇒ Option[B]): Option[List[B]] = {
//    sequence(a map f)
    a.foldRight(Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)(_ :: _))
  }
  
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)

}