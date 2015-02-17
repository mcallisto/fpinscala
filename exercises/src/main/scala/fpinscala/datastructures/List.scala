package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil ⇒ 0 // The sum of the empty list is 0.
    case Cons(x,xs) ⇒ x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x,xs) ⇒ x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) ⇒ x
    case Nil ⇒ 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
    case Cons(h, t) ⇒ h + sum(t)
    case _ ⇒ 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil ⇒ a2
      case Cons(h,t) ⇒ Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = // Utility functions
    as match {
      case Nil ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) ⇒ x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) ⇒ x * y`; see sidebar

  // Exercise 2: Implement `tail`
  def tail[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(x, xs) ⇒ xs
  }

  // Exercise 5: Implement `setHead`, replaces the first element of a List with a different value
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(x, xs) ⇒ Cons(h, xs)
  }

  // Exercise 3: Implement `drop`, removes the first n elements from a list
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil ⇒ Nil
    case end if (n == 0) ⇒ l
    case Cons(x, xs) ⇒ drop(xs, n - 1)
  }

  // Exercise 4: Implement `dropWhile`, removes elements as long as they match a predicate
  def dropWhile[A](l: List[A], f: A ⇒ Boolean): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(x, xs) if f(x) ⇒ dropWhile(xs, f)
    case Cons(y, ys) ⇒ Cons(y, dropWhile(ys, f))
  }

  // Exercise 6: Implement `init`, returns a List consisting of all but the last element of a List
  def init[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(x, Nil) ⇒ Nil
    case Cons(h, t) ⇒ Cons(h, init(t))
  }

  // Exercise 9: Implement `length`, compute the length of a list using foldRight
  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) ⇒ y + 1)

  // Exercise 10: Implement `foldLeft`, another general list-recursion function, tail-recursive
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) ⇒ B): B = as match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ foldLeft(xs, f(z, x))(f)
  }
  
  // Exercise 11:  Write sum, product, and a function to compute the length of a list using foldLeft
  def sumF(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productF(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def lengthF[A](l: List[A]): Int = foldLeft(l, 0)((x, y) ⇒ x + 1)
  
  // Exercise 12: Write a function that returns the reverse of a list
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((x, y) ⇒ Cons(y, x))

  // Exercise 13: Can you write foldLeft in terms of foldRight? And the opposite?
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) ⇒ B): B = foldRight(reverse(as), z)((x, y) ⇒ f(y, x))
  
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) ⇒ B): B = foldLeft(reverse(as), z)((x, y) ⇒ f(y, x))
  
  // Exercise 14: Implement append in terms of either foldLeft or foldRight
  def appendF[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))
  
  // Exercise 15: Write a function that concatenates a list of lists into a single list
  def concatenate[A](ll: List[List[A]]): List[A] = foldLeft(ll, Nil: List[A])(appendF(_, _))
  
  // Exercise 16: Write a function that transforms a list of integers by adding 1 to each element
  def incr(ints: List[Int]): List[Int] = foldRight(ints, Nil: List[Int])((x, y) ⇒ Cons(x + 1, y))

  // Exercise 17: Write a function that turns each value in a List[Double] into a String
  def transf(ds: List[Double]): List[String] =
    foldRight(ds, Nil: List[String])((x, y) ⇒ Cons(x.toString, y))
  
  // Exercise 18: Implement `map`, another general list-recursion function, tail-recursive
  def map[A,B](l: List[A])(f: A ⇒ B): List[B] = foldRight(l, Nil: List[B])((x, y) ⇒ Cons(f(x), y))
  
  // Exercise 19: filter removes elements from a list unless they satisfy a given predicate
  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] = dropWhile(l, (x: A) ⇒ !f(x))

  // Exercise 20: Write a function flatMap
  def flatMap[A, B](l: List[A])(f: A ⇒ List[B]): List[B] = concatenate(map(l)(f))
  
  // Exercise 21: Can you use flatMap to implement filter?
  def filterF[A](l: List[A])(f: A ⇒ Boolean): List[A] = flatMap(l)(x ⇒ if (f(x)) List(x) else List())
  
  // Exercise 22: From two lists construct a new list by adding corresponding elements
  def add(ints1: List[Int], ints2: List[Int]): List[Int] = ints1 match {
    case Nil ⇒ Nil
    case Cons(x, xs) ⇒ ints2 match {
      case Nil ⇒ Nil
      case Cons(y, ys) ⇒ Cons(x + y, add(xs, ys))
    }
  }

  // Exercise 23: Generalize add so that it's not specific to integers or addition
  def combine[A, B, C](a1: List[A], a2: List[B])(f: (A, B) ⇒ C): List[C] = a1 match {
    case Nil ⇒ Nil
    case Cons(x, xs) ⇒ a2 match {
      case Nil ⇒ Nil
      case Cons(y, ys) ⇒ Cons(f(x, y), combine(xs, ys)(f))
    }
  }
  
  // Exercise 24: hasSubsequence checks whether a List contains another List as a subsequence
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = contain(slide(l, length(sub)), sub)

  @annotation.tailrec
  def contain[A](l: List[A], c: A): Boolean = l match {
    case Nil ⇒ false
    case Cons(x, _) if x == c ⇒ true
    case Cons(y, ys) ⇒ contain(ys, c)
  }

  def slide[A](l: List[A], n: Int): List[List[A]] =
    map(range(0, length(l) - n + 1))(x ⇒ slice(l, x, x + n))

  def range(s: Int, e: Int): List[Int] = s match {
    case x if x == e ⇒ Nil
    case _ ⇒ Cons(s, range(s + 1, e))
  }

  def slice[A](l: List[A], s: Int, e: Int): List[A] = take(drop(l, s), e - s)
  
  def take[A](l: List[A], n: Int): List[A] = l match {
    case Nil ⇒ Nil
    case x if n == 0 ⇒ Nil
    case Cons(y, ys) ⇒ Cons(y, take(ys, n - 1))
  }  
  
}